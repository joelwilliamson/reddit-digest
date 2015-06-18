{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | This module defines a function that mails a summary of a reddit to a
-- specified address. It depends on the FromJSON module, which
-- converts JSON represented as a ByteString to a data structure
-- representing Reddit.

module Reddit
( sendDigest ) where


import Network.HTTP
import Control.Applicative((<$>),(<*>),pure)
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Environment(getArgs)
import qualified Data.Text as T
import Formatting
import qualified Data.Text.Internal.Builder as Builder
import qualified Data.Text.Internal.Lazy as Lazy.I
import qualified Data.Text.Lazy as Lazy
import Data.Text.Lazy.Encoding(encodeUtf8,decodeUtf8)
import Data.List
import Data.Maybe
import Network.Mail.Mime
import Network.Stream
import Network.URI

import FromJson


-- This takes a chunk of text that has been HTML encoded and replaces all encoded
-- symbols with their regular representation. It currently only handles '&', '<', '>'
decodeHTML :: T.Text -> T.Text
decodeHTML = foldr ((.) . uncurry T.replace) id
             [("&amp;","&")
             ,("&lt;","<")
             ,("&gt;",">")]

-- Convert each article to only have the important data
summary :: [Article] -> [(T.Text,T.Text)]
summary = map (\Article {title = t, internal = i} -> aux t i)
  where aux t (SelfPost self) = (t,self)
        aux t (Link url _) = (t,url)

-- This takes arguments
--        permalink -> title -> body -> linkToComments (ie permalink)
articleHTMLTemplate = "<div><a href='https://www.reddit.com" %stext% "' ><h3>" %stext% "</h3></a><p>" %stext% "</p><div><a href='https://www.reddit.com" %stext% "'>Comments</a></div></div>"

-- This takes arguments
--        targetURL -> thumbnail -> targetURL -> title -> linkToComments (ie. permalink)
imageHTMLTemplate = "<div><div style='display:inline-block' ><a href='" %stext% "' ><img src='" %stext% "' /></a></div><div style='display:inline-block' ><a href='" %stext% "' ><h3>" %stext% "</h3></a><a href='https://reddit.com" %stext% "' ><div>Comments</div></a></div></div>"

-- If the article is a selfpost, this creates a title and a body.
-- If it is a link, it creates a title, link and thumbnail.
createHTMLArticle :: Article -> Lazy.Text
createHTMLArticle (Article {title, permalink, internal = SelfPost html})
  = format articleHTMLTemplate
    permalink
    title
    (decodeHTML html)
    permalink
createHTMLArticle (Article {title, permalink, internal = Link url thumbnail})
  = format imageHTMLTemplate
    url
    thumbnail
    url
    title
    permalink

-- This takes a block of text, and wraps each line (delimited by \n) with HTML
-- paragraph tags.
paragraphs :: T.Text -> T.Text
paragraphs = T.unlines . map (\l -> "<p>" `T.append` l `T.append` "</p>") . T.lines

-- Given a list of article structs, this formats an HTML message and applies a nice header
createDigest :: [Article] -> Lazy.Text
createDigest a = (++) header
                 . Lazy.pack
                 . unlines
                 . intersperse "<hr>"
                 . map (Lazy.unpack. createHTMLArticle)
                 $ a
  where css = "@media handheld { h2 { color: #f00; } }"
        header = "<style>" ++ css ++ "</style>"
                 ++ "<h2>" ++ Lazy.fromStrict (subreddit $ head a) ++ "</h2>"
        (++) = Lazy.append

-- Create a mail message with the summary of the given reddit in the HTML part,
-- addressed to the given email.
createMessage :: T.Text -> Lazy.Text -> Mail
createMessage address digest =
  Mail
  (Address (Just "Reddit Digest") "digest@joelwilliamson.ca")  -- From
  [Address (Just "Recipient") address] -- To
  [] -- CC
  [Address (Just "Digest Logging") "digest-log@joelwilliamson.ca"] -- BCC
  [("Subject","Reddit Digest")] -- Other headers
  [[Part "text/html" None Nothing [] $ encodeUtf8 digest]]


-- fetchSub takes a uri and returns the json for that sub. It cannot handle the redirects from /r/random
fetchSub :: BS.ByteString -> IO BS.ByteString
fetchSub uri = do
  let uri' = fromMaybe (error "Couldn't parse URI") (parseURI $ BS.unpack uri)
  response <- Network.HTTP.simpleHTTP (Request
                                       uri'
                                       GET
                                       [Header HdrContentLength "0"
                                       ,Header HdrUserAgent "reddit-digest by joelwilliamson"]
                                       "")
  responseCode <- case response of (Left _) -> error "Connection Error"
                                   (Right response') -> getResponseCode response
  case responseCode of (2,0,0) -> getResponseBody response
                       (3,0,2) -> fetchRedirect response
                       x -> error $ "Got response code " ++ show x

fetchRedirect :: Result (Response BS.ByteString) -> IO BS.ByteString
fetchRedirect (Left _) = error "We seem to have a connection error. This shouldn't be possible to reach"
fetchRedirect (Right (Response code reason headers body)) =
  fetchSub
  $ BS.pack
  $ head
  $ map (\ (Header _ content) -> content)
  $ filter (\ (Header name content) -> name == HdrLocation) headers

-- Create a mail action that fetches the given reddit and sends it to the
-- specified email address
sendDigest :: BS.ByteString -> BS.ByteString -> IO ()
sendDigest sub address = do
  putStrLn $ BS.unpack
    $ "Using address: "
    ++ "http://www.reddit.com"
    ++ (if address /= ""
        then "/r/" ++ sub ++"/.json"
        else "/.json")
  result <- fetchSub $ "http://www.reddit.com" ++ (if address /= "" then "/r/" ++ sub ++"/.json" else "/.json")
  renderSendMail
    $ createMessage (Lazy.toStrict $ decodeUtf8 address)
    $ createDigest $ fromText $ Lazy.toStrict $ decodeUtf8 result
  where (++) = BS.append

main = do
  args <- getArgs
  let [sub,address] = args
  sendDigest (BS.pack sub) (BS.pack address)
