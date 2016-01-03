{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where

import           Control.Monad
import           Telegram.API.Bot
import           Test.Hspec
import           Data.Text (Text)
import qualified Data.Text as T
import           Servant.Client
import           Servant.API
import           Network.HTTP.Types.Status
import           System.Environment

main :: IO ()
main = do
    [token, chatId] <- getArgs
    withArgs [] $ hspec (spec (Token (T.pack token)) (T.pack chatId))

spec :: Token -> Text -> Spec
spec token chatId = do
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
      Left FailureResponse { responseStatus = Status { statusMessage = msg } } <-
        sendMessage token (SendMessageRequest "" "test message" Nothing Nothing Nothing)
      msg `shouldBe` "Bad Request"

    it "should send message markdown" $ do
      Right MessageResponse { message_result = m } <-
        sendMessage token (SendMessageRequest chatId "text *bold* _italic_ [github](github.com/klappvisor/telegram-api)" (Just Markdown) Nothing Nothing)
      (text m) `shouldBe` (Just "text bold italic github")

  describe "/sendSticker" $ do
    it "should send sticker" $ do
      Right MessageResponse { message_result = Message { sticker = Just sticker } } <-
        sendSticker token (SendStickerRequest chatId "BQADAgADGgADkWgMAAGXlYGBiM_d2wI" Nothing)
      (sticker_file_id sticker) `shouldBe` "BQADAgADGgADkWgMAAGXlYGBiM_d2wI"

  describe "/forwardMessage" $ do
    it "should forward message" $ do
      Left FailureResponse { responseStatus = Status { statusMessage = msg } } <-
        forwardMessage token (ForwardMessageRequest chatId chatId 123)
      msg `shouldBe` "Bad Request"

  describe "/sendLocation" $ do
    it "should send location" $ do
      Right MessageResponse { message_result = Message { location = Just loc } } <-
        sendLocation token (SendLocationRequest chatId 52.38 4.9 Nothing)
      (latitude loc) `shouldSatisfy` (liftM2 (&&) (> 52) (< 52.4))
      (longitude loc) `shouldSatisfy` (liftM2 (&&) (> 4.89) (< 5))

  describe "/sendChatAction" $ do
    it "should set typing action" $ do
      Right ChatActionResponse { action_result = res} <-
        sendChatAction token (SendChatActionRequest chatId Typing)
      res `shouldBe` True
    it "should find location action" $ do
      Right ChatActionResponse { action_result = res} <-
        sendChatAction token (SendChatActionRequest chatId FindLocation)
      res `shouldBe` True

  describe "/getUpdates" $ do
    it "should get all messages" $ do
      Right UpdatesResponse { update_result = updates} <-
        getUpdates token Nothing Nothing Nothing
      (length updates) `shouldSatisfy` (> 0)
      ((message_id . message) (head updates)) `shouldSatisfy` (> 0)
