{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

{- An article has the following structure:
data Article = Article
                    { subreddit :: Text
                    , author :: Text
                    , score :: Int
                    , over_18 :: Bool
                    , title :: Text
                    , internal :: FancyInternal
                    } deriving Show

data FancyInternal = Article Text Text | SelfPost Text
-}

{-|
This converts a representation of a subreddit from a JSON ByteString or Text
to a convenient data structure.
-}

module FromJson
    ( Article(..)
    , FancyInternal(..)
    , fromText
    , fromByteString
    ) where 

import Data.Aeson
import Control.Applicative((<$>),(<*>),pure)
import qualified Data.Text as T(Text)
import Data.Text.Encoding(encodeUtf8)
import Data.Maybe(fromJust)
import Data.ByteString.Lazy(fromStrict,ByteString)


data Kind = Listing | T3
          deriving Show
instance FromJSON Kind where
  parseJSON (String "t3") =  pure T3
  parseJSON (String "Listing") = pure Listing
  parseJSON _ = error "Didn't recognize kind"
instance ToJSON Kind where
  toJSON T3 = String "t3"
  toJSON Listing = String "Listing"


data Unknown = Unknown
             deriving Show
instance ToJSON Unknown where
  toJSON Unknown = Null
instance FromJSON Unknown where
  parseJSON _ = error "Not an object"


data Reddit = Reddit
              { redditKind :: Kind
              , redditData :: ArticleList
              } deriving Show
instance FromJSON Reddit where
  parseJSON (Object v) = Reddit <$>
                         v .: "kind" <*>
                         v .: "data"
  parseJSON _ = error "Reddit must be an object"
instance ToJSON Reddit where
  toJSON (Reddit k d) = object ["kind" .= k, "data" .= d]

data ArticleList = ArticleList
                   { modhash :: T.Text
                   , children :: [ArticleWrapper]
                   , after :: T.Text
                   , before :: Maybe Unknown
                   } deriving Show
instance FromJSON ArticleList where
  parseJSON (Object v) = ArticleList <$>
                         v .: "modhash" <*>
                         v .: "children" <*>
                         v .: "after" <*>
                         v .:? "before"
  parseJSON _ = fail "Not an article list"
instance ToJSON ArticleList where
  toJSON (ArticleList mod children after before) = object ["modhash" .= mod, "children" .= children]


data ArticleWrapper = ArticleWrapper
               { articleKind :: Kind
               , article :: ArticleRaw
               } deriving Show
instance ToJSON ArticleWrapper where
 toJSON (ArticleWrapper k d) = object ["kind" .= k, "data" .= d]
instance FromJSON ArticleWrapper where
  parseJSON (Object v) = ArticleWrapper <$>
                         v .: "kind" <*>
                         v .: "data"


data ArticleRaw = ArticleRaw
                   { domain' :: T.Text
                   , subreddit' :: T.Text
                   , selftext_html' :: Maybe T.Text
                   , selftext' :: T.Text
                   , author' :: T.Text
                   , score' :: Int
                   , over_18' :: Bool
                   , thumbnail' :: T.Text
                   , isSelf' :: Bool
                   , permalink' :: T.Text
                   , url' :: T.Text
                   , title' :: T.Text
                   } deriving Show
instance FromJSON ArticleRaw where
  parseJSON (Object v) = ArticleRaw <$>
                         v .: "domain"  <*>
                         v .: "subreddit" <*>
                         v .:? "selftext_html" <*>
                         v .: "selftext" <*>
                         v .: "author" <*>
                         v .: "score" <*>
                         v .: "over_18" <*>
                         v .: "thumbnail" <*>
                         v .: "is_self" <*>
                         v .: "permalink" <*>
                         v .: "url" <*>
                         v .: "title"
instance ToJSON ArticleRaw where
  toJSON (ArticleRaw {..}) = object ["domain" .= domain', "title" .= title']

data Article = Article
                    { subreddit :: T.Text
                    , author :: T.Text
                    , score :: Int
                    , over_18 :: Bool
                    , title :: T.Text
                    , permalink :: T.Text
                    , internal :: FancyInternal
                    } deriving Show
-- A selfpost has the selftext_html, a link has the url and thumbnail
data FancyInternal = SelfPost T.Text | Link T.Text T.Text deriving Show

toADT :: ArticleRaw -> Article
toADT (ArticleRaw {..})
  | selftext' == "" = withCommon $ Link url' thumbnail'
  | otherwise = withCommon $ SelfPost $ fromJust selftext_html'
  where withCommon fi = Article {subreddit = subreddit'
                                     , author = author'
                                     , score = score'
                                     , over_18 = over_18'
                                     , permalink = permalink'
                                     , title = title'
                                     , internal = fi}

articleList :: Maybe Reddit -> [Article]
articleList Nothing = []
articleList (Just (Reddit {redditData = ArticleList { children = l }})) = map (toADT . article) l

fromText :: T.Text -> [Article]
fromText = articleList . decode . fromStrict . encodeUtf8

fromByteString :: ByteString -> [Article]
fromByteString = articleList . decode
