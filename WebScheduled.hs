{-# LANGUAGE OverloadedStrings #-}
-- | This is the entry point to the program. It establishes the TChans that pass
-- information around, starts the web server and SQL backend, then runs the simple
-- scheduler.

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
import System.Random(randomIO)
import Data.Int(Int64)
import Control.Applicative(pure)
import Debug.Trace(trace)

import Data.Maybe(fromMaybe,isJust)

-- Create an authentication token. The secret key should be provided at program
-- start, then the resulting function can be used whenever necessary.
type AuthorizationGenerator = BS.L.ByteString ->
                              BS.L.ByteString ->
                              BS.L.ByteString ->
                              String
hmac :: BS.L.ByteString -> AuthorizationGenerator
hmac secretKey freq addr sub = showDigest
                     $ hmacSha1 secretKey
                     $ freq `BS.L.append` addr `BS.L.append` sub


bslLookup assoc key= BS.L.fromStrict <$> join (lookup key assoc)

-- Given a query, check if the authorization is valid, and if it is then construct
-- a schedule entry for it.
convert :: AuthorizationGenerator ->
           Query ->
           Maybe (ScheduleEntry (Char8.ByteString,
                                 Char8.ByteString,
                                 Char8.ByteString,
                                 Bool))
convert authGen l = join $ liftM5 aux freq addr sub auth reauth
  where freq = read <$> Char8.unpack <$> join (lookup "freq" l)
        freqS = bslLookup l "freq"
        addr = bslLookup l "addr"
        sub = bslLookup l "sub"
        mobile = isJust $ lookup "mobile" l
        auth = Char8.unpack <$> join (lookup "auth" l)
        reauth :: Maybe String
        reauth = authGen <$> freqS <*> addr <*> sub
        aux freq addr sub auth reauth  = if auth == reauth
          then Just ScheduleEntry { freq = freq
                                    , action = sendDigest (MessageType mobile) sub addr
                                    , key = (Char8.pack $ show freq
                                           ,BS.L.toStrict addr
                                           ,BS.L.toStrict sub
                                           ,mobile)}
          else trace "Authorization failed" Nothing

mail :: AuthorizationGenerator -> WebInterface.Mail
mail authGen l = fromMaybe (return ()) (sendAuthMessage <$> addr <*> sub <*> freq <*> auth <*> pure mobile)
  where addr = bslLookup l "addr"
        freq = bslLookup l "freq"
        sub = bslLookup l "sub"
        mobile = isJust $ lookup "mobile" l
        auth = BS.L.C8.pack <$> (authGen <$> freq <*> addr <*> sub)

sendAuthMessage :: BS.L.ByteString ->
                  BS.L.ByteString ->
                  BS.L.ByteString ->
                  BS.L.ByteString ->
                  Bool ->
                  IO ()
sendAuthMessage addr sub freq auth mobile = do
  mail <- simpleMail (Address Nothing $ Strict.decodeUtf8 $ BS.L.toStrict addr)
          (Address (Just "Reddit Digest") "digest@joelwilliamson.ca")
          "Authentication" ""
          ("<a href='http://www.joelwilliamson.ca:3000/?"
           ++ "freq=" ++ decodeUtf8 freq
           ++ "&addr=" ++ decodeUtf8 addr
           ++ "&sub=" ++ decodeUtf8 sub
           ++ "&auth=" ++ decodeUtf8 auth
           ++ if mobile then "&mobile" else ""
           ++ "' >Click link to authenticate</a>")
          []
  renderSendMail mail
  where (++) = Data.Text.Lazy.append

--checkQueue :: (Frequency, IO (),()
-- These are dummy jobs used to trigger a check for new jobs
checkQueue :: ScheduleEntry (Char8.ByteString, Char8.ByteString, Char8.ByteString,Bool)
checkQueue = ScheduleEntry { freq = Minute
                           , action = return ()
                           , key = ("","","",False)}

buildSched = do
  now <- getCurrentTime
  return
    $ foldl (\q n -> PQ.insert
                     (addUTCTime (10*n) now)
                     checkQueue q)
    PQ.empty [1,2..6] -- We need to specify the step size because these are
    -- some fractional type. We would get very small steps otherwise

main = do
  authGen <- hmac . BS.L.fromStrict . Char8.pack . show <$> (randomIO :: IO Int64)
  schedulerChan <- atomically newTChan
  restore schedulerChan
  saverChan <- atomically $ dupTChan schedulerChan
  forkIO $ web 3000 (convert authGen) (mail authGen) schedulerChan
  forkIO $ save saverChan
  sched <- buildSched
  let current = S.fromList $ PQ.elemsU sched
  runJobs (sched,current,S.empty) schedulerChan
