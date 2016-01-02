{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where

import Control.Monad
import Telegram.API.Bot
import Telegram.API.Bot.Data
import Telegram.API.Bot.Responses
import Telegram.API.Bot.Requests
import Data.Text
import Test.Hspec

main :: IO ()
main = hspec spec

token = Token "bot179176211:AAGtOVb_YxcpK8FaJ-ixpdOKsEEy-7LINH0"

spec :: Spec
spec = do
  describe "/getMe" $ do
    it "responds with correct bot's name" $ do
      Right GetMeResponse { user_result = u } <-
        getMe token
      (user_first_name u) `shouldBe` "TelegramAPIBot"

  describe "/sendMessage" $ do
    it "should send message" $ do
          Right SendMessageResponse { message_result = m } <-
            sendMessage token (SendMessageRequest "3331366" "test message" Nothing Nothing Nothing)
          (text m) `shouldBe` (Just "test message")

    it "should send message markdown" $ do
      Right SendMessageResponse { message_result = m } <-
        sendMessage token (SendMessageRequest "3331366" "test *message*" (Just Markdown) Nothing Nothing)
      (text m) `shouldBe` (Just "test *message*")

  describe "/sendSticker" $ do
      it "should send sticker" $ do
            Right SendMessageResponse { message_result = m } <-
              sendSticker token (SendStickerRequest "3331366" "BQADAgADGgADkWgMAAGXlYGBiM_d2wI" Nothing)
            (text m) `shouldBe` (Just "test message")
