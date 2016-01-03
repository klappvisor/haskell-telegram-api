{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where

import           Control.Monad
import           Telegram.API.Bot
import           Telegram.API.Bot.Data
import           Telegram.API.Bot.Responses
import           Telegram.API.Bot.Requests
import           Test.Hspec
import           Data.Text (Text)
import qualified Data.Text as T
import           Servant.Client
import           Servant.API

main :: IO ()
main = hspec spec

token = Token "bot179176211:AAGtOVb_YxcpK8FaJ-ixpdOKsEEy-7LINH0"
chatId = "3331366"
message = "text *bold* _italic_ [github](github.com/klappvisor/telegram-api)"

spec :: Spec
spec = do
  describe "/getMe" $ do
    it "responds with correct bot's name" $ do
      Right GetMeResponse { user_result = u } <-
        getMe token
      (user_first_name u) `shouldBe` "TelegramAPIBot"

  describe "/sendMessage" $ do
    it "should send message" $ do
      Right MessageResponse { message_result = m } <-
        sendMessage token (SendMessageRequest chatId "test message" Nothing Nothing Nothing)
      (text m) `shouldBe` (Just "test message")

    it "should be error message" $ do
      Left FailureResponse { responseStatus = status } <-
        sendMessage token (SendMessageRequest "" "test message" Nothing Nothing Nothing)
      putStrLn (show status)-- (statusMessage status) `shouldBe` "Bad Request"

    it "should send message markdown" $ do
      Right MessageResponse { message_result = m } <-
        sendMessage token (SendMessageRequest chatId message (Just Markdown) Nothing Nothing)
      (text m) `shouldBe` (Just "text bold italic github")
      (message_id m) `shouldBe` (0)

  describe "/sendSticker" $ do
    it "should send sticker" $ do
      Right MessageResponse { message_result = m } <-
        sendSticker token (SendStickerRequest chatId "BQADAgADGgADkWgMAAGXlYGBiM_d2wI" Nothing)
      (text m) `shouldBe` Nothing

  describe "/forwardMessage" $ do
    it "should forward message" $ do
      Right MessageResponse { message_result = m } <-
        forwardMessage token (ForwardMessageRequest chatId "anotherChatId" 123)
      (text m) `shouldBe` Nothing

  describe "/sendLocation" $ do
    it "should send location" $ do
      Right MessageResponse { message_result = m } <-
        sendLocation token (SendLocationRequest chatId 11.0 12.0 Nothing)
      (latitude . location m) `shouldBe` 11.0