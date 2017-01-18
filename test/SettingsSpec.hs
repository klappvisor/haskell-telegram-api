{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module SettingsSpec (spec) where

import           Data.Text               (Text)
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Test.Hspec
import           Web.Telegram.API.Bot

spec :: Token -> Text -> Spec
spec token botName = do
  manager <- runIO $ newManager tlsManagerSettings
  describe "/getMe and /getWebhookInfo" $
    it "responds with correct bot's name and empty webhook" $ do
      res <- runClient (do
          b <- getMeM
          info <- getWebhookInfoM
          return (b, info)) token manager
      let Right (Response {result = me}, Response {result = whi}) = res
      user_first_name me `shouldBe` botName
      whi_url whi `shouldBe` ""
