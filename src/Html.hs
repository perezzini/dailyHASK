{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}

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

import User
import Location
import Interest
import News
-- import Weather
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
  url = News.getArticleUrl article
  urlToImage = handleNull article News.getArticleUrlToImage
  publishedAt = handleNull article News.getArticlePublishedAt
  in H.div $ do
    H.article $ do
      h2 $ toHtml $ title
      h3 $ toHtml $ description
      p $ toHtml $ url

-- currentWeatherToHtml :: Weather -> Html
-- currentWeatherToHtml weather = let
--   temp = show $ Weather.getTemp weather
--   pressure = show $ Weather.getPressure weather
--   humidity = show $ Weather.getHumidity weather
--   in H.div $ do
--     h4 "The current weather: "
--     ul $ do
--       li $ toHtml $ "Temperature: " ++ temp
--       li $ toHtml $ "Pressure: " ++ pressure
--       li $ toHtml $ "Humidity: " ++ humidity

welcomeMailTemplate :: User -> Html
welcomeMailTemplate user = docTypeHtml $ do
  H.head $ do
    H.title "Welcome to dailyHASK"
  body $ do
    userInfoToHtml user

renderWelcomeMailTemplate :: User -> String
renderWelcomeMailTemplate user = renderHtml $ welcomeMailTemplate user

dailyMailTemplate :: News -> Html
dailyMailTemplate news = let
  total = News.getNewsTotal news
  totalStr = show total :: String
  totalHeader = "Total articles: " ++ totalStr
  articles = News.getNewsArticles news
  in if total == 0
    then H.div $ do
      h3 "Application couldn't retrieve news articles matching your interests today."
    else docTypeHtml $ do
      H.head $ do
        H.title "dailyHASK"
      body $ do
        h2 "The following news articles match your interests and were published today"
        h3 $ toHtml $ totalHeader
        ul $ forM_ articles (li . articleToHtml)

renderDailyMailTemplate :: News -> String
renderDailyMailTemplate news = renderHtml $ dailyMailTemplate news
