{-# LANGUAGE OverloadedStrings #-}

module Html
    (
    renderWelcomeMailTemplate
    ) where

import Prelude hiding (span)

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String

import Control.Monad (forM_)
import Data.Text as Text hiding (unwords)

import User
import Location
import Interest
import News

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
  in H.div ! A.id "user-info" $ do
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
articleToHtml = undefined

welcomeMailTemplate :: User -> Html
welcomeMailTemplate user = docTypeHtml $ do
  H.head $ do
    H.title "Welcome to dailyHASK"
  body $ do
    userInfoToHtml user

renderWelcomeMailTemplate :: User -> String
renderWelcomeMailTemplate user = renderHtml $ welcomeMailTemplate user

dailyMailTemplate = undefined
