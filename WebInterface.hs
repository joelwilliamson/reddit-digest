{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module WebInterface
( web
, Convertor(..)
, Mail(..)
) where

import Network.Wai
import Network.HTTP.Types
import Network.HTTP.Types.URI
import Network.Wai.Handler.Warp

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as Char8
import Control.Concurrent.STM.TChan
import Control.Monad.STM (atomically)
import Control.Concurrent
import Debug.Trace

import Paths_reddit_digest

-- Render a HTTP request to HTML. This is mostly useful for debugging, as it
-- shows what the server thinks was requested
displayRequest :: Request -> Lazy.ByteString
displayRequest req =
  "<html>\n<head>\n<title>Request Successful</title>\n<head>\n<body>\n"
  +++ foldl (\acc l -> acc +++ "<p>" +++ l +++ "</p>\n") "" (Char8.lines $ showQuery $ queryString req)
  +++ "</body>\n</html>\n"
  where (+++) = Lazy.append

showQuery :: Query -> Lazy.ByteString
showQuery = Char8.unlines . map showQueryItem

showQueryItem :: QueryItem -> Lazy.ByteString
showQueryItem (name,Nothing) = Lazy.fromStrict name
showQueryItem (name, Just value) = Lazy.fromStrict name `Lazy.append` " = " `Lazy.append` Lazy.fromStrict value

sendSubscribeForm respond = do
  path <- getDataFileName "SubscriptionForm.html"  
  respond $ responseFile status200 [("Content-Type","text/html")] path Nothing

-- A Convertor takes a query and tries to convert it to some value of type a.
-- In actual practice, this creates a schedule entry, or fails if the
-- authentication is invalid.
type Convertor a = Query -> Maybe a
type Mail = Query -> IO () -- This IO should be sending an authentication link

-- Create a webserver IO that handles authentication and writes any requests
-- onto a provided TChan.
application :: Convertor a -> Mail -> TChan a -> Application
application convert mail chan req respond =
  case queryString req of
    [] -> sendSubscribeForm respond
    _ ->
      case lookup "auth" $ queryString req of
        Nothing -> sendAuthMessage
        Just _ ->
          case convert $ queryString req of
            Just req' -> goodRequest req'
            Nothing -> badRequest
  where sendAuthMessage = do
          trace "No auth. Sending mail." $ return ()
          mail $ queryString req
          respond $
            responseLBS status202 [("Content-Type","text/html")]
            $ displayRequest req
        goodRequest req' = do
              atomically $ writeTChan chan req'
              respond $
                responseLBS status200 [("Content-Type","text/html")]
                $ displayRequest req
        badRequest = respond
          $ responseLBS status400 [("Content-Type","text/html")]
          $ "<html><head><title>400 Bad Request</title></head><body><h2>400 Bad Request</h2></body></html>"



-- This packages the query string for any request made on the given port
-- and puts the query into a channel. It also sends the query back to the
-- foreign host
web :: Port -> Convertor a -> Mail -> TChan a -> IO ()
web port convert mail chan = run port $ application convert mail chan

displayChan chan = do
  chanContents <- atomically $ readTChan chan
  print chanContents
  displayChan chan
