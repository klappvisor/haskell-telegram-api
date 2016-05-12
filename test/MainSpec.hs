{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}

module MainSpec (spec) where

import           Control.Monad
import           Web.Telegram.API.Bot
import           Test.Hspec
import           Data.Either (isRight, isLeft)
import           Data.Text (Text)
import qualified Data.Text as T
import           Servant.Client
import           Servant.API
import           Network.HTTP.Types.Status
import           System.Environment
import           System.FilePath

import           Paths_telegram_api

-- to print out remote response if response success not match
success, nosuccess :: (Show a, Show b) =>Either a b ->Expectation
success   e = e `shouldSatisfy` isRight
nosuccess e = e `shouldSatisfy` isLeft

spec :: Token -> Text -> Text -> Spec
spec token chatId botName = do
  describe "/getMe" $ do
    it "responds with correct bot's name" $ do
      Right GetMeResponse { user_result = u } <-
        getMe token
      (user_first_name u) `shouldBe` botName -- "TelegramAPIBot"

  describe "/sendMessage" $ do
    it "should send message" $ do
      res <-sendMessage token (SendMessageRequest chatId "test message" Nothing Nothing Nothing Nothing)
      success res
      let Right MessageResponse { message_result = m } = res
      (text m) `shouldBe` (Just "test message")

    it "should return error message" $ do
      res <-sendMessage token (SendMessageRequest "" "test message" Nothing Nothing Nothing Nothing)
      nosuccess res
      let Left FailureResponse { responseStatus = Status { statusMessage = msg } } = res
      msg `shouldBe` "Bad Request"

    it "should send message markdown" $ do
      res <-sendMessage token (SendMessageRequest chatId "text *bold* _italic_ [github](github.com/klappvisor/telegram-api)" (Just Markdown) Nothing Nothing Nothing)
      success res
      let Right MessageResponse { message_result = m } = res
      (text m) `shouldBe` (Just "text bold italic github")

    it "should set keyboard" $ do
      res <-sendMessage token (SendMessageRequest chatId "set keyboard" Nothing Nothing Nothing (Just (ReplyKeyboardMarkup [["A", "B"], ["C"]] Nothing Nothing Nothing)))
      success res
      let Right MessageResponse { message_result = m } = res
      (text m) `shouldBe` (Just "set keyboard")

    it "should remove keyboard" $ do
      res <-sendMessage token (SendMessageRequest chatId "remove keyboard" Nothing Nothing Nothing (Just (ReplyKeyboardHide True Nothing)))
      success res
      let Right MessageResponse { message_result = m } = res
      (text m) `shouldBe` (Just "remove keyboard")

    it "should force reply" $ do
      res <-sendMessage token (SendMessageRequest chatId "force reply" Nothing Nothing Nothing (Just (ForceReply True Nothing)))
      success res
      let Right MessageResponse { message_result = m } = res
      (text m) `shouldBe` (Just "force reply")

  describe "/forwardMessage" $ do
    it "should forward message" $ do
      res <-forwardMessage token (ForwardMessageRequest chatId chatId 123)
      nosuccess res
      let Left FailureResponse { responseStatus = Status { statusMessage = msg } } = res
      msg `shouldBe` "Bad Request"

  describe "/sendPhoto" $ do
    it "should return error message" $ do
      Left FailureResponse { responseStatus = Status { statusMessage = msg } } <-
        sendPhotoById token (SendPhotoRequest "" "photo_id" (Just "photo caption") Nothing Nothing)
      msg `shouldBe` "Bad Request"
    it "should send photo with file_id" $ do
      Right MessageResponse { message_result = Message { caption = Just cpt } } <-
        sendPhotoById token (SendPhotoRequest chatId "AgADBAADv6cxGybVMgABtZ_EOpBSdxYD5xwZAAQ4ElUVMAsbbBqFAAIC" (Just "photo caption") Nothing Nothing)
      cpt `shouldBe` "photo caption"
    it "should send uploaded photo" $ do
      dataDir <- getDataDir
      let fileUpload = FileUpload "image/jpeg" (FileUploadFile (dataDir </> "test-data/klappvisor.jpg"))
      Right MessageResponse { message_result = Message { caption = Just cpt } } <-
        sendPhoto token (SendPhotoRequest chatId fileUpload (Just "photo caption 2") Nothing Nothing)
      cpt `shouldBe` "photo caption 2"

  describe "/sendAudio" $ do
    it "should return error message" $ do
      Left FailureResponse { responseStatus = Status { statusMessage = msg } } <-
        sendAudio token (SendAudioRequest "" "audio_id" Nothing (Just "performer") (Just "title") Nothing Nothing)
      msg `shouldBe` "Bad Request"
--         it "should send audio" $ do
--           Right MessageResponse { message_result = Message { audio = Just Audio { audio_title = Just title } } } <-
--             sendAudio token (SendAudioRequest chatId "audio_id" Nothing (Just "performer") (Just "my title 1") Nothing)
--           title `shouldBe` "my title 1"

  describe "/sendSticker" $ do
    it "should send sticker" $ do
      Right MessageResponse { message_result = Message { sticker = Just sticker } } <-
        sendSticker token (SendStickerRequest chatId "BQADAgADGgADkWgMAAGXlYGBiM_d2wI" Nothing Nothing)
      (sticker_file_id sticker) `shouldBe` "BQADAgADGgADkWgMAAGXlYGBiM_d2wI"

  describe "/sendLocation" $ do
    it "should send location" $ do
      Right MessageResponse { message_result = Message { location = Just loc } } <-
        sendLocation token (SendLocationRequest chatId 52.38 4.9 Nothing Nothing)
      (latitude loc) `shouldSatisfy` (liftM2 (&&) (> 52) (< 52.4))
      (longitude loc) `shouldSatisfy` (liftM2 (&&) (> 4.89) (< 5))

  describe "/sendChatAction" $ do
    it "should set typing action" $ do
      Right ChatActionResponse { action_result = res} <-
        sendChatAction token (SendChatActionRequest chatId Typing)
      res `shouldBe` True
    it "should set find location action" $ do
      Right ChatActionResponse { action_result = res} <-
        sendChatAction token (SendChatActionRequest chatId FindLocation)
      res `shouldBe` True
    it "should set upload photo action" $ do
      Right ChatActionResponse { action_result = res} <-
        sendChatAction token (SendChatActionRequest chatId UploadPhoto)
      res `shouldBe` True

  describe "/getUpdates" $ do
    it "should get all messages" $ do
      Right UpdatesResponse { update_result = updates} <-
        getUpdates token Nothing Nothing Nothing
      (length updates) `shouldSatisfy` (>= 0)

  describe "/getFile" $ do
    it "should get file" $ do
      Right FileResponse { file_result = file } <-
        getFile token "AAQEABMXDZEwAARC0Kj3twkzNcMkAAIC"
      (fmap (T.take 10) (file_path file)) `shouldBe` (Just "thumb/file")

    it "should return error" $ do
      Left FailureResponse { responseStatus = Status { statusMessage = msg } } <-
        getFile token "AAQEABMXDZEwAARC0Kj3twkzNcMkAmm"
      msg `shouldBe` "Bad Request"

  describe "/getUserProfilePhotos" $ do
    it "should get user profile photos" $ do
      Right UserProfilePhotosResponse { photos_result = photos } <-
        getUserProfilePhotos token (read (T.unpack chatId)) Nothing Nothing
      (total_count photos) `shouldSatisfy` (> 0)

  describe "/setWebhook" $ do
    it "should set webhook" $ do
      Right SetWebhookResponse { webhook_result = res } <-
        setWebhook token (Just "https://example.com/secret_token")
      res `shouldBe` True

    it "should remove webhook" $ do
      Right SetWebhookResponse { webhook_result = res } <-
        setWebhook token Nothing
      res `shouldBe` True
