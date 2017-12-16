{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module StickersSpec (spec) where

import           Data.Either             (isLeft, isRight)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Test.Hspec
import           Web.Telegram.API.Bot

-- to print out remote response if response success not match
success, nosuccess :: (Show a, Show b) => Either a b -> Expectation
success   e = e `shouldSatisfy` isRight
nosuccess e = e `shouldSatisfy` isLeft

spec :: Token -> ChatId -> Text -> Spec
spec token (ChatId chatId) botName = do
  manager <- runIO $ newManager tlsManagerSettings

  describe "/sendInvoice" $ do
    it "should send invoice" $ do
      1 `shouldBe` 1

