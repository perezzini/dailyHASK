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
    , Publisher(..)
    , Sources(..)
    , getNews
    , getSources
    , getNewsTotal
    , getNewsArticles
    , getArticleSourceName
    , getArticleAuthor
    , getArticleTitle
    , getArticleDescripton
    , getArticleUrl
    , getArticleUrlToImage
    , getArticlePublishedAt
    , getPublisherID
    , getPublisherUrl
    , getPublisherName
    , getPublisherCountry
    , getPublisherLanguage
    , getPublisherCategory
    , getPublisherDescription
    , getTotalPublishers
    , getPublishers
    ) where

import Config
import Interest
import Http
import Url
import Date
import Utils
import Error as E

import Data.Text as Text hiding (length)
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

data Publisher = Publisher {
  publisherID :: Text
  , name :: Maybe Text
  , sourceDescription :: Maybe Text
  , sourceUrl :: Url
  , category :: Maybe Text
  , language :: Maybe Text
  , country :: Maybe Text
} deriving (Show)

-- Just retrieve the entire JSON object
instance FromJSON Publisher where
  parseJSON = withObject "Publisher" $ \v -> do
    id <- v .: "id"
    name <- v .: "name"
    description <- v .: "description"
    url <- v .: "url"
    category <- v .: "category"
    language <- v .: "language"
    country <- v .: "country"
    return (Publisher id name description url category language country)

data Sources = Sources {
  publishers :: [Publisher]
  , totalPublishers :: Int
} deriving (Show)

instance FromJSON Sources where
  parseJSON = withObject "Sources" $ \v -> do
    publishers <- v .: "sources"
    let totalPublishers = length publishers
    return (Sources publishers totalPublishers)

-- |The 'getNewsTotal' function takes a 'News' value and returns the 'total' value from it
getNewsTotal :: News -> Int
getNewsTotal = total

-- |The 'getNewsArticles' function takes a 'News' value and returns the 'articles' value from it
getNewsArticles :: News -> [Article]
getNewsArticles = articles

-- |The 'getArticleSourceName' function takes an 'Article' value and maybe returns its source name. It
-- returns 'Nothing' in case of the value is JSON null.
getArticleSourceName :: Article -> Maybe Text
getArticleSourceName = sourceName

-- |The 'getArticleAuthor' function takes an 'Article' value and maybe returns its author. It
-- returns 'Nothing' in case of the value is JSON null.
getArticleAuthor :: Article -> Maybe Text
getArticleAuthor = author

-- |The 'getArticleTitle' function takes an 'Article' value and maybe returns its title. It
-- returns 'Nothing' in case of the value is JSON null.
getArticleTitle :: Article -> Maybe Text
getArticleTitle = title

-- |The 'getArticleDescripton' function takes an 'Article' value and maybe returns its description. It
-- returns 'Nothing' in case of the value is JSON null.
getArticleDescripton :: Article -> Maybe Text
getArticleDescripton = description

-- |The 'getArticleUrl' function takes an 'Article' value and returns the URL pointing to the corresponding article. It
-- returns 'Nothing' in case of the value is JSON null.
getArticleUrl :: Article -> Url
getArticleUrl = url

-- |The 'getArticleUrlToImage' function takes an 'Article' value and maybe returns its image URL. It
-- returns 'Nothing' in case of the value is JSON null.
getArticleUrlToImage :: Article -> Maybe Url
getArticleUrlToImage = urlToImage

-- |The 'getArticlePublishedAt' function takes a 'Article' value and maybe returns the date-time it was published. It
-- returns 'Nothing' in case of the value is JSON null.
getArticlePublishedAt :: Article -> Maybe Text
getArticlePublishedAt = publishedAt

-- |The 'getPublisherID' function takes a 'Publisher' value and returns its ID
getPublisherID :: Publisher -> Text
getPublisherID = publisherID

-- |The 'getPublisherID' function takes a 'Publisher' value and maybe returns its ID. It
-- returns 'Nothing' in case of the value is JSON null.
getPublisherName :: Publisher -> Maybe Text
getPublisherName = name

-- |The 'getPublisherDescription' function takes a 'Publisher' value and maybe returns its description. It
-- returns 'Nothing' in case of the value is JSON null.
getPublisherDescription :: Publisher -> Maybe Text
getPublisherDescription = sourceDescription

-- |The 'getPublisherUrl' function takes a 'Publisher' value and returns its 'Url'
getPublisherUrl :: Publisher -> Url
getPublisherUrl = sourceUrl

-- |The 'getPublisherCategory' function takes a 'Publisher' value and maybe returns its category. It
-- returns 'Nothing' in case of the value is JSON null.
getPublisherCategory :: Publisher -> Maybe Text
getPublisherCategory = category

-- |The 'getPublisherLanguage' function takes a 'Publisher' value and maybe returns its ID. It
-- returns 'Nothing' in case of the value is JSON null.
getPublisherLanguage :: Publisher -> Maybe Text
getPublisherLanguage = language

-- |The 'getPublisherCountry' function takes a 'Publisher' value and maybe returns the country where its from. It
-- returns 'Nothing' in case of the value is JSON null.
getPublisherCountry :: Publisher -> Maybe Text
getPublisherCountry = country

-- |The 'getPublishers' function takes 'Sources' and returns the entire list of publishers
getPublishers :: Sources -> [Publisher]
getPublishers = publishers

-- |The 'getTotalPublishers' function takes 'Sources' and returns how many publishers there are
getTotalPublishers :: Sources -> Int
getTotalPublishers = totalPublishers

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

-- |The 'getNews' function takes a list of interests, a parameter about how to sort retrieved news, and wanted language.
-- It returns a value of type 'News' mathing these interests, and specified language, or 'Nothing' in case the GET
-- request to NewsAPI.org's API fails. The news are from today and sorted by specified parameter.
-- Explore NewsAPI.org website for more information about the 'Everything' endpoint parameters
getNews :: [Interest] -> Text -> Text -> IO (Maybe News)
getNews interests sort lang = do
  putStrLn "[getNews] Start of GET request from news API..."
  endpoint <- endpoint "api.news.endpoint.everything"
  key <- key
  today <- Date.today
  let today' = Text.pack today
  let interests' = Text.pack $ Utils.connectListOfStrings (Interest.fromDataType interests) " OR "
  let opts = defaults & param "apiKey" .~ [key]
          & param "q" .~ [interests']
          & param "sortBy" .~ [sort]
          & param "language" .~ [lang]
          & param "from" .~ [today']
  req <- getWith opts (Text.unpack endpoint)
  let headerStatusCode = req ^. responseStatus . statusCode
  let apiStatus = req ^. responseBody . Lens.key "status" . Lens._String
  putStrLn "[getNews] End of GET request from news API"
  if Http.isGETRequestOk headerStatusCode && apiRequestOk apiStatus
    then return (decode $ req ^. responseBody)
    else return $ Nothing

-- |The 'getSources' function takes a specific category, language, and country, and maybe returns the corresponding
-- sources making a GET request to NewsAPI.org's API. If this fails, it'll return 'Nothing'.
-- Explore NewsAPI.org website for more information about the 'Sources' endpoint parameters
getSources :: Text -> Text -> Text -> IO (Maybe Sources)
getSources category language country = do
  putStrLn "[getSources] Start of GET request from news API..."
  endpoint <- endpoint "api.news.endpoint.sources"
  key <- key
  let opts = defaults & param "apiKey" .~ [key]
          & param "category" .~ [category]
          & param "language" .~ [language]
          & param "country" .~ [country]
  req <- getWith opts (Text.unpack endpoint)
  let headerStatusCode = req ^. responseStatus . statusCode
  let apiStatus = req ^. responseBody . Lens.key "status" . Lens._String
  putStrLn "[getSources] End of GET request from news API"
  if Http.isGETRequestOk headerStatusCode && apiRequestOk apiStatus
    then return (decode $ req ^. responseBody)
    else return $ Nothing
