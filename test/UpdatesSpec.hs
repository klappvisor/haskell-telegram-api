{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module UpdatesSpec (spec) where

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Data.Text               (Text)
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Test.Hspec
import           Web.Telegram.API.Bot

spec :: Token -> Text -> Spec
spec token botName = do
  manager <- runIO $ newManager tlsManagerSettings
  describe "/getMe and /getWebhookInfo work together" $
    it "responds with correct bot's name and empty webhook" $ do
      res <- runClient ( do
          b <- getMeM
          info <- getWebhookInfoM
          return (b, info)) token manager
      let Right (Response {result = me}, Response {result = whi}) = res
      user_first_name me `shouldBe` botName
      whi_url whi `shouldBe` ""
  describe "webhook operations" $
    it "able to get and set webhook" $ do
      res <- runClient ( do
          info <- getWebhookInfoM
          liftIO $ (whi_url . result) info `shouldBe` ""
          liftIO $ threadDelay $ 2 * 1000 * 1000 -- to avoid Too many request error
          set <- setWebhookM $ Just "https://example.com/bot"
          liftIO $ result set `shouldBe` True
          liftIO $ threadDelay $ 2 * 1000 * 1000 -- to avoid Too many request error
          del <- deleteWebhookM
          liftIO $ result del `shouldBe` True
          getWebhookInfoM) token manager
      let Right Response { result = whi } = res
      whi_url whi `shouldBe` ""
