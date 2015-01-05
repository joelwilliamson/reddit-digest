module Persist (save,restore) where

import Database.HDBC
import Database.HDBC.Sqlite3

import Control.Concurrent.STM.TChan
import Control.Monad.STM(atomically)
import Control.Monad(liftM)
import Data.ByteString.Char8
import Data.List as L

import SimpleScheduler
import Reddit

import Debug.Trace

checkerS conn = prepare conn "select * from users where addr = ? and sub = ? and freq = ?"

-- This is a bit hacky. To enable deleting, assume that a request to add an
-- entry that already exists is actually a request to delete the entry.
-- I still need to remove the entry from the live schedule.
saveTChan :: (IConnection c) =>
             TChan (ScheduleEntry (ByteString,ByteString,ByteString))
             -> c
             -> Statement
             -> IO ()
saveTChan chan conn stmt = do
  ScheduleEntry { key = (freq,addr,sub)} <- atomically $ readTChan chan
  let sqlArg = [toSql addr, toSql sub, toSql freq]
  checkStmt <- checkerS conn
  deleteStmt <- prepare conn "delete from users where addr = ? and sub = ? and freq = ?"
  execute checkStmt sqlArg
  found <- fetchRow checkStmt
  case found of 
    Nothing -> trace "Adding to database" $  execute stmt sqlArg
    Just _ -> trace "Removing from database" $ execute deleteStmt sqlArg
  commit conn
  saveTChan chan conn stmt

-- Prepare a statement to insert a new user into the database
insertS :: Connection -> IO Statement
insertS conn = prepare conn "insert into users (addr, sub, freq) values (?, ?, ?)"

-- Prepare a statement to get all the users.
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
save :: TChan (ScheduleEntry (ByteString, ByteString, ByteString)) -> IO ()
save chan = do
  conn <- openConnection "users.db"
  stmt <- insertS conn
  saveTChan chan conn stmt

restore :: TChan (ScheduleEntry (ByteString, ByteString, ByteString)) -> IO ()
restore chan = do
  conn <- openConnection "users.db"
  stmt <- selectS conn
  execute stmt []
  rows <- fetchAllRows stmt
  mapM_ (atomically . writeTChan chan)
    $ L.map ( \ [a,s,f] ->
               ScheduleEntry { freq = read $ fromSql f :: Frequency,
                               action = sendDigest (fromSql s) (fromSql a),
                               key = (fromSql f, fromSql a, fromSql s)}) rows
  disconnect conn
