{-|
Module      : News

Definition of 'News' and 'Articles' data types along multiple functions to handle them.
Defines a GET request to retrieve news from NewsApi.org services that match multiple interests
and were published today at some time
-}


{-# LANGUAGE OverloadedStrings #-}

module News
    (
    News(..)
    , Article(..)
    , getNews
    , getNewsTotal
    , getNewsArticles
    , getArticleSourceName
    , getArticleAuthor
    , getArticleTitle
    , getArticleDescripton
    , getArticleUrl
    , getArticleUrlToImage
    , getArticlePublishedAt
    ) where

import Config
import Interest
import Http
import Url
import Date
import Utils
import Error as E

import Data.Text as Text
import Data.Maybe as M

import Data.Aeson
import qualified Data.Aeson.Lens as Lens (key, _String)
import Network.Wreq
import Control.Lens

data Article = Article {
  sourceName :: Maybe Text
  , author :: Maybe Text
  , title :: Maybe Text
  , description :: Maybe Text
  , url :: Url
  , urlToImage :: Maybe Url
  , publishedAt :: Maybe Text
} deriving (Show)

-- Here is where we parse the GET request response object.
-- We just want the following fields: "source" (and "name"
-- from inside it), "author", "title", "description", "url",
-- "urlToImage", and "publishedAt"
instance FromJSON Article where
  parseJSON = withObject "Article" $ \v -> do
    sourceObject <- v .: "source"
    sourceName <- sourceObject .: "name"
    author <- v .: "author"
    title <- v .: "title"
    description <- v .: "description"
    url <- v .: "url"
    urlToImage <- v .: "urlToImage"
    publishedAt <- v .: "publishedAt"
    return (Article sourceName author title description url urlToImage publishedAt)

data News = News {
  total :: Int
  , articles :: [Article]
} deriving (Show)

-- Just retrieve the entire JSON object
instance FromJSON News where
  parseJSON = withObject "News" $ \v -> do
    total <- v .: "totalResults"
    articles <- v .: "articles"
    return (News total articles)

-- |The 'getNewsTotal' function takes a 'News' value and returns the 'total' value from it
getNewsTotal :: News -> Int
getNewsTotal = total

-- |The 'getNewsArticles' function takes a 'News' value and returns the 'articles' value from it
getNewsArticles :: News -> [Article]
getNewsArticles = articles

-- |The 'getArticleSourceName' function takes an 'Article' value and maybe returns it's source name. It
-- returns 'Nothing' in case of the value is JSON null.
getArticleSourceName :: Article -> Maybe Text
getArticleSourceName = sourceName

-- |The 'getArticleAuthor' function takes an 'Article' value and maybe returns it's author. It
-- returns 'Nothing' in case of the value is JSON null.
getArticleAuthor :: Article -> Maybe Text
getArticleAuthor = author

-- |The 'getArticleTitle' function takes an 'Article' value and maybe returns it's title. It
-- returns 'Nothing' in case of the value is JSON null.
getArticleTitle :: Article -> Maybe Text
getArticleTitle = title

-- |The 'getArticleDescripton' function takes an 'Article' value and maybe returns it's description. It
-- returns 'Nothing' in case of the value is JSON null.
getArticleDescripton :: Article -> Maybe Text
getArticleDescripton = description

-- |The 'getArticleUrl' function takes an 'Article' value and returns the URL pointing to the corresponding article. It
-- returns 'Nothing' in case of the value is JSON null.
getArticleUrl :: Article -> Url
getArticleUrl = url

-- |The 'getArticleUrlToImage' function takes an 'Article' value and maybe returns it's image URL. It
-- returns 'Nothing' in case of the value is JSON null.
getArticleUrlToImage :: Article -> Maybe Url
getArticleUrlToImage = urlToImage

-- |The 'getArticlePublishedAt' function takes an 'Article' value and maybe returns the date-time it was published. It
-- returns 'Nothing' in case of the value is JSON null.
getArticlePublishedAt :: Article -> Maybe Text
getArticlePublishedAt = publishedAt

endpoint :: String -> IO Url
endpoint e = do
  value <- Config.getValue e
  if M.isNothing value
    then let
      errorMsg = "Error: " ++ e ++ " config value not found"
      in E.callError errorMsg
    else return $ Text.pack $ M.fromJust value

key :: IO Text
key = do
  value <- Config.getValue "api.news.key"
  if M.isNothing value
    then E.callError "Error: api.news.key not found"
    else return $ Text.pack $ M.fromJust value

apiRequestOk :: Text -> Bool
apiRequestOk t = if t == "OK" || t == "ok"
  then True
  else False

-- |The 'getNews' function takes a list of interests and returns: a value of type 'News' mathing these interests,
-- or 'Nothing' in case the GET request to NewsApi.org fails. The news are from today and sorted by popularity
getNews :: [Interest] -> IO (Maybe News)
getNews interests = do
  putStrLn "Start of GET request from news API..."
  endpoint <- endpoint "api.news.endpoint.everything"
  key <- key
  today <- Date.today
  let today' = Text.pack today
  let interests' = Text.pack $ Utils.connectListOfStrings (Interest.fromDataType interests) " OR "
  let opts = defaults & param "apiKey" .~ [key]
          & param "q" .~ [interests']
          & param "sortBy" .~ ["popularity"]
          & param "language" .~ ["en"]
          & param "from" .~ [today']
  req <- getWith opts (Text.unpack endpoint)
  let headerStatusCode = req ^. responseStatus . statusCode
  let apiStatus = req ^. responseBody . Lens.key "status" . Lens._String
  putStrLn "End of GET request from news API"
  if Http.isGETRequestOk headerStatusCode && apiRequestOk apiStatus
    then return (decode $ req ^. responseBody)
    else return $ Nothing
