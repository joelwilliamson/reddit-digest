{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | This module defines a function that mails a summary of a reddit to a
-- specified address. It depends on the FromJSON module, which converts JSON\
-- represented as a ByteString to a data structure representing Reddit.
module Reddit
( MessageType(..), sendDigest ) where


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

data MessageType = MessageType {
  mobile :: Bool
  } deriving (Eq,Ord,Show)

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
--        subdomain -> permalink -> title -> body -> subdomain -> linkToComments (ie permalink)
articleHTMLTemplate = "<div><a href='https://"
                      %stext%".reddit.com/" -- subdomain
                      %stext% "' ><h3>" -- permalink
                      %stext% "</h3></a><p>" -- title
                      %stext% "</p><div><a href='https://" --body
                      %stext%".reddit.com" --subdomain
                      %stext% "'>Comments</a></div></div>" -- comments

-- This takes arguments
--        targetURL -> thumbnail -> targetURL -> title -> subdomain -> inkToComments (ie. permalink)
imageHTMLTemplate = "<div><div style='display:inline-block' ><a href='"
                    %stext% "' ><img src='" -- subdomain
                    %stext% "' /></a></div><div style='display:inline-block' ><a href='" -- thumbnail
                    %stext% "' ><h3>" -- targetURL
                    %stext% "</h3></a><a href='https://" -- title
                    %stext%".reddit.com" -- subdomain
                    %stext% "' ><div>Comments</div></a></div></div>" -- comments

-- If the article is a selfpost, this creates a title and a body.
-- If it is a link, it creates a title, link and thumbnail.
createHTMLArticle :: MessageType -> Article -> Lazy.Text
createHTMLArticle mt (Article {title, permalink, internal = SelfPost html})
  = format articleHTMLTemplate
    subdomain
    permalink
    title
    (decodeHTML html)
    subdomain
    permalink
  where subdomain = if mobile mt then "m" else "www"
createHTMLArticle mt (Article {title, permalink, internal = Link url thumbnail})
  = format imageHTMLTemplate
    url
    thumbnail
    url
    title
    subdomain
    permalink
  where subdomain = if mobile mt then "m" else "www"

-- This takes a block of text, and wraps each line (delimited by \n) with HTML
-- paragraph tags.
paragraphs :: T.Text -> T.Text
paragraphs = T.unlines . map (\l -> "<p>" `T.append` l `T.append` "</p>") . T.lines

-- Given a list of article structs, this formats an HTML message and applies a nice header
createDigest :: MessageType -> [Article] -> Lazy.Text
createDigest mt a = (++) header
                 . Lazy.pack
                 . unlines
                 . intersperse "<hr>"
                 . map (Lazy.unpack. createHTMLArticle mt)
                 $ a
  where header = "<h2>" ++ Lazy.fromStrict (subreddit $ head a) ++ "</h2>"
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
sendDigest :: MessageType -> BS.ByteString -> BS.ByteString -> IO ()
sendDigest mt sub address = do
  putStrLn $ BS.unpack
    $ "Using address: "
    ++ "http://www.reddit.com"
    ++ (if address /= ""
        then "/r/" ++ sub ++"/.json"
        else "/.json")
  result <- fetchSub $ "http://www.reddit.com" ++ (if address /= "" then "/r/" ++ sub ++"/.json" else "/.json")
  renderSendMail
    $ createMessage (Lazy.toStrict $ decodeUtf8 address)
    $ createDigest mt $ fromText $ Lazy.toStrict $ decodeUtf8 result
  where (++) = BS.append
