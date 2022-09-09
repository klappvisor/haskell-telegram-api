{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module UpdatesSpec (spec) where

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Test.Hspec
import           TestCore
import           Web.Telegram.API.Bot

spec :: Token -> Text -> Spec
spec token botName = do
  manager <- runIO $ newManager tlsManagerSettings
  describe "/getMe and /getWebhookInfo work together" $
    it "responds with correct bot's name and empty webhook" $ do
      res@(Right (Response {result = me}, Response {result = whi})) <-
        runTelegramClient token manager $ do
          b <- getMeM
          info <- getWebhookInfoM
          return (b, info)
      success res
      user_first_name me `shouldBe` botName
      whi_url whi `shouldBe` ""
  describe "webhook operations" $ do
    let url = "https://example.com/bot"
    it "able to get and set webhook" $ do
      res@(Right Response { result = whi }) <-
        runTelegramClient token manager $ do
          info1 <- getWebhookInfoM
          liftIO $ (whi_url . result) info1 `shouldBe` ""
          liftIO $ threadDelay $ 2 * 1000 * 1000 -- to avoid Too many request error
          set <- setWebhookM $ setWebhookRequest' url
          liftIO $ result set `shouldBe` True
          info2 <- getWebhookInfoM
          liftIO $ (whi_url . result) info2 `shouldBe` url
          liftIO $ threadDelay $ 2 * 1000 * 1000 -- to avoid Too many request error
          del <- deleteWebhookM
          liftIO $ result del `shouldBe` True
          getWebhookInfoM
      success res
      whi_url whi `shouldBe` ""

    it "should set allowed updates" $ do
      let allowedUpdates = map T.pack ["message", "callback_query"]
      res@(Right Response { result = whi }) <-
        runTelegramClient token manager $ do
          let request =
                case setWebhookRequest' url of
                  SetWebhookWithoutCertRequest {..} ->
                    SetWebhookWithoutCertRequest {
                      webhook_allowed_updates = Just allowedUpdates, ..
                    }
                  r -> r
          set <- setWebhookM request
          liftIO $ result set `shouldBe` True
          getWebhookInfoM
      success res
      whi_allowed_updates whi `shouldBe` Just allowedUpdates

    it "should set max connections" $ do
      res@(Right Response { result = whi }) <-
        runTelegramClient token manager $ do
          let request = (setWebhookRequest' url) {
              webhook_max_connections = Just 5
          }
          liftIO $ threadDelay $ 2 * 1000 * 1000 -- to avoid Too many request error
          set <- setWebhookM request
          liftIO $ result set `shouldBe` True
          getWebhookInfoM
      success res
      whi_max_connections whi `shouldBe` Just 5


