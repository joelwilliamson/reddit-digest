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

type Convertor a = Query -> Maybe a
type Mail = Query -> IO () -- This IO should be sending a authentication link

application :: Convertor a -> Mail -> TChan a -> Application
application convert mail chan req respond =
  case lookup "auth" $ queryString req of
    Nothing -> do
      trace "No auth. Sending mail." $ return ()
      mail $ queryString req
      respond $
        responseLBS status202 [("Content-Type","text/html")]
        $ displayRequest req
    Just _ ->
      trace "Got auth. Handling..." $ case convert $ queryString req of
        Just req' -> do
          atomically $ writeTChan chan req'
          respond $
            responseLBS status200 [("Content-Type","text/html")]
            $ displayRequest req
        Nothing -> respond $
                   responseLBS status400 [("Content-Type","text/html")]
                   "<html><head><title>400 Bad Request</title></head><body><h2>400 Bad Request</h2></body></html>"

-- This packages the query string for any request made on the given port
-- and puts the query into a channel. It also sends the query back to the
-- foreign host
web :: Port -> Convertor a -> Mail -> TChan a -> IO ()
web port convert mail chan = run port $ application convert mail chan

displayChan chan = do
  chanContents <- atomically $ readTChan chan
  print chanContents
  displayChan chan
