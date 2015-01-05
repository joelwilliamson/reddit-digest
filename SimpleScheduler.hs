{-# LANGUAGE OverloadedStrings #-}
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

data Frequency = Minute | Hour | Day | Week
               deriving (Show)
instance Read Frequency where
  readsPrec _ = readsPrec . map toLower
  readsPrec' "minute" = [(Minute,"")]
  readsPrec' "hour" = [(Hour,"")]
  readsPrec' "day" = [(Day,"")]
  readsPrec' "week" = [(Week,"")]
  readsPrec' str = error $ "Can't read frequency " ++ str-}

type ScheduleEntry a = (Frequency, IO (), a)

-- Each element in the schedule is keyed by when it should next run.
type Schedule a = PQ.MinPQueue UTCTime (ScheduleEntry a)


addJobsFromChan :: TChan (ScheduleEntry a)
                   -> UTCTime
                   -> Schedule a
                   -> STM (Schedule a)
addJobsFromChan chan now sched = do
  nextJob <- tryReadTChan chan
  case nextJob of Nothing -> return sched
                  Just job -> addJobsFromChan chan now
                              $ PQ.insert now job sched

nextScheduled :: Frequency -> UTCTime -> UTCTime
nextScheduled Minute = addUTCTime 60
nextScheduled Hour = addUTCTime 3600
nextScheduled Day = addUTCTime 86400
nextScheduled Week = addUTCTime $ 86400*7

nextJob :: Schedule a -> (IO (), UTCTime, Schedule a)
nextJob sched = (firstJob, nextSched, sched')
  where (scheduledFor, j@(firstJobSched, firstJob,_)) = PQ.findMin sched
        nextTime = nextScheduled firstJobSched scheduledFor
        sched' = PQ.insert nextTime j $ PQ.deleteMin sched
        (nextSched,_) = PQ.findMin sched'

runJobs :: Schedule a -> TChan (ScheduleEntry a) -> IO ()
runJobs sched chan = do
  currentTime <- getCurrentTime
  sched' <- atomically $ addJobsFromChan chan currentTime sched
  let (firstJob, nextSched, sched'') = nextJob sched' in
    if PQ.null sched'
    then do
      threadDelay 1000000
      runJobs sched' chan
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
