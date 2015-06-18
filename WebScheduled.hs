{-# LANGUAGE OverloadedStrings #-}

import WebInterface
import SimpleScheduler
import Reddit
import Persist

import Network.HTTP.Types.URI
import Network.Mail.Mime

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as BS.L
import qualified Data.ByteString.Lazy.Char8 as BS.L.C8(pack)
import Data.Text.Lazy.Encoding(decodeUtf8)
import qualified Data.Text.Encoding as Strict(decodeUtf8)
import Data.Text.Lazy(append)

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Concurrent(forkIO)
import qualified Data.PQueue.Prio.Min as PQ
import Data.Time
import Control.Applicative((<$>),(<*>))
import Control.Monad(liftM5,join)
import Data.Digest.Pure.SHA(hmacSha1,showDigest)
import qualified Data.Set as S

import Data.Maybe(fromMaybe)

hmac :: BS.L.ByteString ->
        BS.L.ByteString ->
        BS.L.ByteString ->
        String
hmac freq addr sub = showDigest
                     $ hmacSha1 "SECRET: THIS SHOULD CHANGE"
                     $ freq `BS.L.append` addr `BS.L.append` sub


bslLookup assoc key= BS.L.fromStrict <$> join (lookup key assoc)

convert :: Query -> Maybe (ScheduleEntry (Char8.ByteString,
                                          Char8.ByteString,
                                          Char8.ByteString))
convert l = join $ liftM5 aux freq addr sub auth reauth
  where freq = read <$> Char8.unpack <$> join (lookup "freq" l)
        freqS = bslLookup l "freq"
        addr = bslLookup l "addr"
        sub = bslLookup l "sub"
        auth = Char8.unpack <$> join (lookup "auth" l)
        reauth :: Maybe String
        reauth = hmac <$> freqS <*> addr <*> sub
        aux freq addr sub auth reauth  = if auth == reauth
          then Just ScheduleEntry { freq = freq
                                    , action = sendDigest sub addr
                                    , key = (Char8.pack $ show freq
                                           ,BS.L.toStrict addr
                                           ,BS.L.toStrict sub)}
          else Nothing

mail :: WebInterface.Mail
mail l = fromMaybe (return ()) (sendAuthMessage <$> addr <*> sub <*> freq <*> auth)
  where addr = bslLookup l "addr"
        freq = bslLookup l "freq"
        sub = bslLookup l "sub"
        auth = BS.L.C8.pack <$> (hmac <$> freq <*> addr <*> sub)

sendAuthMessage :: BS.L.ByteString ->
                  BS.L.ByteString ->
                  BS.L.ByteString ->
                  BS.L.ByteString ->
                  IO ()
sendAuthMessage addr sub freq auth = do
  mail <- simpleMail (Address Nothing $ Strict.decodeUtf8 $ BS.L.toStrict addr)
          (Address (Just "Reddit Digest") "digest@joelwilliamson.ca")
          "Authentication" ""
          ("<a href='http://www.joelwilliamson.ca:3000/?"
           ++ "freq=" ++ decodeUtf8 freq
           ++ "&addr=" ++ decodeUtf8 addr
           ++ "&sub=" ++ decodeUtf8 sub
           ++ "&auth=" ++ decodeUtf8 auth
           ++ "' >Click link to authenticate</a>")
          []
  renderSendMail mail
  where (++) = Data.Text.Lazy.append

--checkQueue :: (Frequency, IO (),()
-- These are dummy jobs used to trigger a check for new jobs
checkQueue :: ScheduleEntry (Char8.ByteString, Char8.ByteString, Char8.ByteString)
checkQueue = ScheduleEntry { freq = Minute
                           , action = return ()
                           , key = ("","","")}

buildSched = do
  now <- getCurrentTime
  return
    $ foldl (\q n -> PQ.insert
                     (addUTCTime (10*n) now)
                     checkQueue q)
    PQ.empty [1,2..6] -- We need to specify the step size because these are
    -- some fractional type. We would get very small steps otherwise

main = do
  schedulerChan <- atomically newTChan
  restore schedulerChan
  saverChan <- atomically $ dupTChan schedulerChan
  forkIO $ web 3000 convert mail schedulerChan
  forkIO $ save saverChan
  sched <- buildSched
  let current = S.fromList $ PQ.elemsU sched
  runJobs (sched,current,S.empty) schedulerChan
