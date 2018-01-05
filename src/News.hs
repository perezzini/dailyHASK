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
import Json
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

instance FromJSON News where
  parseJSON = withObject "News" $ \v -> do
    total <- v .: "totalResults"
    articles <- v .: "articles"
    return (News total articles)

getNewsTotal :: News -> Int
getNewsTotal = total

getNewsArticles :: News -> [Article]
getNewsArticles = articles

getArticleSourceName :: Article -> Maybe Text
getArticleSourceName = sourceName

getArticleAuthor :: Article -> Maybe Text
getArticleAuthor = author

getArticleTitle :: Article -> Maybe Text
getArticleTitle = title

getArticleDescripton :: Article -> Maybe Text
getArticleDescripton = description

getArticleUrl :: Article -> Url
getArticleUrl = url

getArticleUrlToImage :: Article -> Maybe Url
getArticleUrlToImage = urlToImage

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

getNews :: [Interest] -> IO (Maybe News)
getNews interests = do
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
  if Json.httpRequestOk headerStatusCode && apiRequestOk apiStatus
    then return (decode $ req ^. responseBody)
    else return $ Nothing
