{-|
Module      : Html

Definition of functions to render HTML output code
-}

{-# LANGUAGE OverloadedStrings #-}

module Html
    (
    renderWelcomeMailTemplate
    , renderDailyMailTemplate
    ) where

import Prelude hiding (span)
import Control.Monad (forM_)

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String

import Control.Monad (forM_)
import Data.Text as Text hiding (unwords)
import Data.String

import User
import Location
import Interest
import News
import Weather
import Utils

userInfoToHtml :: User -> Html
userInfoToHtml user = let
  title = "Welcome to dailyHASK!"
  subtitle = "The following is your personal data"
  userFirstName = Text.unpack $ getFirstName $ User.getName user :: String
  userLastName = Text.unpack $ getLastName $ User.getName user :: String
  userEmail = Text.unpack $ User.getEmail user :: String
  userGeoLoc = User.getLocation user
  userLocationAddress = Text.unpack $ Location.getAddress userGeoLoc :: String
  userInterests = unwords $ Interest.fromDataType $ User.getInterests user :: String
  in H.div $ do
    article $ do
      header $ do
        h1 title
        h3 subtitle
        ul $ do
          li $ toHtml $ "First name: " ++ userFirstName
          li $ toHtml $ "Last name: " ++ userLastName
          li $ toHtml $ "Email address: " ++ userEmail
          li $ toHtml $ "Geographic address: " ++ userLocationAddress
          li $ toHtml $ "Interests: " ++ userInterests

articleToHtml :: Article -> Html
articleToHtml article = let
  handleNull article f = either Prelude.id Prelude.id $ Utils.handleNullValue $ f article

  sourceName = handleNull article News.getArticleSourceName
  author = handleNull article News.getArticleAuthor
  title = handleNull article News.getArticleTitle
  description = handleNull article News.getArticleDescripton
  url = fromString $ Text.unpack $ News.getArticleUrl article :: AttributeValue
  urlToImage = handleNull article News.getArticleUrlToImage
  publishedAt = handleNull article News.getArticlePublishedAt
  in H.div $ do
    H.article $ do
      h4 $ b $ a ! href url $ toHtml title
      h5 $ b $ toHtml $ sourceName
      h5 ! A.style "color:#A9A9A9" $ toHtml $ author
      h5 $ i $ toHtml $ description

currentWeatherToHtml :: Weather -> Html
currentWeatherToHtml weather = let
  handleNull weather f = either Prelude.id Prelude.id $ Utils.handleNullValue $ f weather
  (temp, pressure, humidity, description) = (show $ round $ Weather.kelvinToCelsius $ Weather.getTemp weather
                                            , show $ Weather.getPressure weather
                                            , show $ Weather.getHumidity weather
                                            , Text.unpack $ handleNull weather Weather.getDescription)
  in H.div $ do
    h3 $ toHtml $ "The current weather (" ++ description ++ "): "
    h4 $ toHtml $ "Temperature: " ++ temp ++ " °C; " ++ "pressure: " ++ pressure ++ " hPa; " ++ "humidity: " ++ humidity ++ " %"

welcomeMailTemplate :: User -> Html
welcomeMailTemplate user = docTypeHtml $ do
  H.head $ do
    H.title "Welcome to dailyHASK"
  body $ do
    userInfoToHtml user

-- |The 'renderWelcomeMailTemplate' returns a string containing HTML code corresponding to a welcome-mail template
renderWelcomeMailTemplate :: User -> String
renderWelcomeMailTemplate user = renderHtml $ welcomeMailTemplate user

dailyMailTemplate :: User -> News -> Weather -> Html
dailyMailTemplate user news weather = let
  total = News.getNewsTotal news
  totalStr = show total :: String
  totalHeader = "total articles: " ++ totalStr
  articles = News.getNewsArticles news

  userFirstName = Text.unpack $ User.getFirstName $ User.getName user
  in if total == 0
    then H.div $ do
      h3 "Application couldn't retrieve news articles matching your interests today."
    else docTypeHtml $ do
      H.head $ do
        H.title "dailyHASK"
      body $ do
        currentWeatherToHtml weather
        h3 $ toHtml $ userFirstName ++ ", the following news articles match your interests and were published today"
        ul $ forM_ articles (li . articleToHtml)

-- |The 'renderDailyMailTemplate' returns a string containing HTML code corresponding to the daily-mail template
renderDailyMailTemplate :: User -> News -> Weather -> String
renderDailyMailTemplate user news weather = renderHtml $ dailyMailTemplate user news weather
