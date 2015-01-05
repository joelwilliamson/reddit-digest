{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SimpleScheduler
( Frequency(..)
, ScheduleEntry(..)
, Schedule(..)
, runJobs
) where 

import qualified Data.PQueue.Prio.Min as PQ
import Data.Time(UTCTime,getCurrentTime,diffUTCTime,NominalDiffTime(..),addUTCTime)
import Control.Concurrent(forkIO,threadDelay)
import Control.Concurrent.STM.TChan(TChan,tryReadTChan,newTChan)
import Control.Monad.STM(STM,atomically)

import Data.ByteString(ByteString)
import Data.Char(toLower)
import qualified Data.Set as S

data Frequency = Minute | Hour | Day | Week
               deriving (Show,Ord,Eq)
instance Read Frequency where
  readsPrec _ = readsPrec' . map toLower
    where readsPrec' "minute" = [(Minute,"")]
          readsPrec' "hour" = [(Hour,"")]
          readsPrec' "day" = [(Day,"")]
          readsPrec' "week" = [(Week,"")]
          readsPrec' str = error $ "Can't read frequency " ++ str

data ScheduleEntry a = ScheduleEntry { freq :: Frequency
                                     , action :: IO ()
                                     , key :: a}

instance (Eq a) => Eq (ScheduleEntry a) where
  x == y = key x == key y
instance (Ord a) => Ord (ScheduleEntry a) where
  x `compare` y = key x `compare` key y

-- Each element in the schedule is keyed by when it should next run.
-- The schedule will consist of three structures. A priority queue will hold the
-- next time the job is due to run, and two sets will hold the current elements
-- and the elements that should be deleted. When a job is going to be run, the
-- deletion set is checked, and if the job is found, it is deleted from both
-- structs. When a job comes over the channel, both channels are checked. If the
-- job is in the deletion set, it is removed from the deletion set and add to
-- the current element set. If the job is in the current element set, it is
-- removed from that set and added to the delete set. If the job is in neither
-- set, it is added to the curent element set **and the job queue**.
-- type Sched a = (Job queue, current elements, delete elements)
type Schedule a = (PQ.MinPQueue UTCTime (ScheduleEntry a), S.Set (ScheduleEntry a), S.Set (ScheduleEntry a))


addJobsFromChan :: (Ord a)
                   => TChan (ScheduleEntry a)
                   -> UTCTime
                   -> Schedule a
                   -> STM (Schedule a)
addJobsFromChan chan now sched@(queue,current,delete) = do
  nextJob <- tryReadTChan chan
  case nextJob of
       Nothing -> return sched
       Just job -> addJobsFromChan chan now
                   $ case (S.member job current, S.member job delete) of
                     (False,False) ->  (PQ.insert now job queue
                                       , S.insert job current
                                       , delete)
                     (False,True) -> (queue
                                      , S.insert job current
                                      , S.delete job delete)
                     (True,False) -> (queue
                                      , S.delete job current
                                      , S.insert job delete)
                     (True,True) -> error "A job should never be in both the current and delete sets"
                                      
nextScheduled :: Frequency -> UTCTime -> UTCTime
nextScheduled Minute = addUTCTime 60
nextScheduled Hour = addUTCTime 3600
nextScheduled Day = addUTCTime 86400
nextScheduled Week = addUTCTime $ 86400*7

--Return the next job, the time it should run at, and the new schedule
nextJob :: Ord a => Schedule a -> (IO (), UTCTime, Schedule a)
nextJob (sched,current,delete) = (action j, nextSched, (sched',current,delete))
  where (scheduledFor, j) = PQ.findMin sched
        nextTime = nextScheduled (freq j) scheduledFor
        sched' = if S.member j delete
                 then PQ.deleteMin sched
                 else PQ.insert nextTime j $ PQ.deleteMin sched
        (nextSched,_) = PQ.findMin sched'

runJobs :: Ord a => Schedule a -> TChan (ScheduleEntry a) -> IO ()
runJobs sched chan = do
  currentTime <- getCurrentTime
  s@(sched',_,_) <- atomically $ addJobsFromChan chan currentTime sched
  let (firstJob, nextSched, sched'') = nextJob s in
    if PQ.null $ sched'
    then do
      threadDelay 1000000
      runJobs s chan
    else do
      forkIO firstJob
      return ()
      currentTime <- getCurrentTime
      threadDelay $ (*) 1000000 $ floor $ diffUTCTime nextSched currentTime
      runJobs sched'' chan :: IO ()
  
-- Testing stuff
{-
job1 = (Minute, putStrLn "Job1","Job1")
job2 = (Minute, putStrLn "Job2","Job2")
job3 = (Minute, putStrLn "Job3","Job3")

sched12 = do
  now <- getCurrentTime
  chan <- atomically newTChan
  return (runJobs (PQ.insert now job1
                   $ PQ.insert (addUTCTime 30 now) job2
                   $ PQ.empty) chan
          , chan)
-}
