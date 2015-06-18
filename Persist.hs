-- | This module is responsible for interacting with the backing database. The
-- rest of the program communicates with it over a pair of TChans. The first
-- TChan is used by the `restore` function, which reads the database and sends
-- all the schedule entry over the chan. The reciever must then construct a
-- schedule to actually run the jobs.
-- `save` holds its chan open, and whenever it recieves a request, it toggles
-- the state of the particular subscription in the DB. If the subscription
-- already existed, it is removed, and if it didn't it is added.
module Persist (save,restore) where

import Database.HDBC
import Database.HDBC.Sqlite3

import Control.Concurrent.STM.TChan
import Control.Monad.STM(atomically)
import Control.Monad(liftM)
import Data.ByteString.Char8
import Data.List as L
import Data.Time(UTCTime(..))
import Data.Time.Clock(secondsToDiffTime)
import Data.Time.Calendar.WeekDate(toWeekDate)

import SimpleScheduler
import Reddit

import Debug.Trace


-- Return how far into the given period a certain time is
-- E.g. Day (3:00 February 7) --> 3
--    Month (February 7) --> 7
currentTime :: Frequency -> UTCTime -> Int
currentTime Minute t = floor (utctDayTime t) `mod` 60
currentTime Hour t = (floor (utctDayTime t) `mod` 3600) `div` 60
currentTime Day t = floor (utctDayTime t) `div` 3600
currentTime Week t = (\(_,_,a) -> a) . toWeekDate $ utctDay t

-- | `save` holds its chan open, and whenever it recieves a request, it toggles
-- the state of the particular subscription in the DB. If the subscription
-- already existed, it is removed, and if it didn't it is added.
saveTChan :: (IConnection c) =>
             TChan (ScheduleEntry (ByteString,ByteString,ByteString,Bool))
             -> c
             -> Statement
             -> Statement
             -> Statement
             -> IO ()
saveTChan chan conn insert check delete = do
-- This is a bit hacky. To enable deleting, assume that a request to add an
-- entry that already exists is actually a request to delete the entry.
-- I still need to remove the entry from the live schedule.
  ScheduleEntry { key = (freq,addr,sub,mobile)} <- atomically $ readTChan chan
  let sqlArg = [toSql addr, toSql sub, toSql freq, toSql mobile]
  execute check sqlArg
  found <- fetchRow check
  case found of 
    Nothing -> trace "Adding to database" $  execute insert sqlArg
    Just _ -> trace "Removing from database" $ execute delete sqlArg
  commit conn
  saveTChan chan conn insert check delete

-- All these statements are used in the save functionality
-- Prepare a statement to insert a new user into the database
insertS :: Connection -> IO Statement
insertS conn = prepare conn "insert into users (addr, sub, freq, mobile) values (?, ?, ?, ?)"
checkS conn = prepare conn "select * from users where addr = ? and sub = ? and freq = ? "
deleteS conn = prepare conn "delete from users where addr = ? and sub = ? and freq = ? "

-- This is only used to restore the server from disk
selectS conn = prepare conn "select * from users"

-- Open the database. If it doesn't already exist, make it.
openConnection :: FilePath -> IO Connection
openConnection path = do
  conn <- connectSqlite3 path
  tables <- getTables conn
  if "users" `L.elem` tables
    then return conn
    else do
         run conn "create table users (addr varchar(256) not null, sub varchar(256) not null, freq varchar(10) not null)" []
         commit conn
         return conn

-- Connect a TChan to the database
save :: TChan (ScheduleEntry (ByteString, ByteString, ByteString,Bool)) -> IO ()
save chan = do
  conn <- openConnection "users.db"
  ins <- insertS conn
  check <- checkS conn
  del <- deleteS conn
  saveTChan chan conn ins check del

-- Read from the database, and send every element in it over the TChan to the
-- scheduler.
restore :: TChan (ScheduleEntry (ByteString, ByteString, ByteString,Bool)) -> IO ()
restore chan = do
  conn <- openConnection "users.db"
  stmt <- selectS conn
  execute stmt []
  rows <- fetchAllRows stmt
  trace (show rows) $ return ()
  mapM_ ((atomically . writeTChan chan)
    . ( \ [a,s,f,mobile] ->
               ScheduleEntry { freq = read $ fromSql f :: Frequency,
                               action = sendDigest (MessageType $ fromSql mobile) (fromSql s) (fromSql a),
                               key = (fromSql f, fromSql a, fromSql s, fromSql mobile)})) rows
  disconnect conn
