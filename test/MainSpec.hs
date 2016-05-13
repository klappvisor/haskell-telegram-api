{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module MainSpec (spec) where

import           Control.Monad
import           Data.Monoid
import           Web.Telegram.API.Bot
import           Test.Hspec
import           Data.Either (isRight, isLeft)
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Servant.Client
import           Network.HTTP.Types.Status

-- to print out remote response if response success not match
success, nosuccess :: (Show a, Show b) =>Either a b ->Expectation
success   e = e `shouldSatisfy` isRight
nosuccess e = e `shouldSatisfy` isLeft

spec :: Token -> Text -> Text -> Spec
spec token chatId botName = do
  manager <- runIO $ newManager tlsManagerSettings
  describe "/getMe" $ do
    it "responds with correct bot's name" $ do
      Right GetMeResponse { user_result = u } <-
        getMe token manager
      (user_first_name u) `shouldBe` botName -- f.e. "TelegramAPIBot"

  describe "/sendMessage" $ do
    it "should send message" $ do
      res <- sendMessage token (SendMessageRequest chatId "test message" Nothing Nothing Nothing Nothing) manager
      success res 
      let Right MessageResponse { message_result = m } = res
      (text m) `shouldBe` (Just "test message")

    it "should return error message" $ do
      res <- sendMessage token (SendMessageRequest "" "test message" Nothing Nothing Nothing Nothing) manager
      nosuccess res
      let Left FailureResponse { responseStatus = Status { statusMessage = msg } } = res
      msg `shouldBe` "Bad Request"

    it "should send message markdown" $ do
      res <- sendMessage token (SendMessageRequest chatId "text *bold* _italic_ [github](github.com/klappvisor/telegram-api)" (Just Markdown) Nothing Nothing Nothing) manager
      success res
      let Right MessageResponse { message_result = m } = res
      (text m) `shouldBe` (Just "text bold italic github")

    it "should set keyboard" $ do
      res <- sendMessage token (SendMessageRequest chatId "set keyboard" Nothing Nothing Nothing (Just (ReplyKeyboardMarkup [["A", "B"], ["C"]] Nothing Nothing Nothing))) manager
      success res
      let Right MessageResponse { message_result = m } = res
      (text m) `shouldBe` (Just "set keyboard")

    it "should remove keyboard" $ do
      res <- sendMessage token (SendMessageRequest chatId "remove keyboard" Nothing Nothing Nothing (Just (ReplyKeyboardHide True Nothing))) manager
      success res
      let Right MessageResponse { message_result = m } = res
      (text m) `shouldBe` (Just "remove keyboard")

    it "should force reply" $ do
      res <- sendMessage token (SendMessageRequest chatId "force reply" Nothing Nothing Nothing (Just (ForceReply True Nothing))) manager
      success res
      let Right MessageResponse { message_result = m } = res
      (text m) `shouldBe` (Just "force reply")

  describe "/forwardMessage" $ do
    it "should forward message" $ do
      res <- forwardMessage token (ForwardMessageRequest chatId chatId 123) manager
      nosuccess res
      let Left FailureResponse { responseStatus = Status { statusMessage = msg } } = res
      msg `shouldBe` "Bad Request"

  describe "/sendPhoto" $ do
    it "should return error message" $ do
      Left FailureResponse { responseStatus = Status { statusMessage = msg } } <-
        sendPhoto token (SendPhotoRequest "" "photo_id" (Just "photo caption") Nothing Nothing) manager
      msg `shouldBe` "Bad Request"
    it "should send photo" $ do
     Right MessageResponse { message_result = Message { caption = Just cpt } } <-
       sendPhoto token (SendPhotoRequest chatId catPic (Just "photo caption") Nothing Nothing) manager
     cpt `shouldBe` "photo caption"

  describe "/sendAudio" $ do
    it "should return error message" $ do
      Left FailureResponse { responseStatus = Status { statusMessage = msg } } <-
        sendAudio token (SendAudioRequest "" "audio_id" Nothing (Just "performer") (Just "title") Nothing Nothing) manager
      msg `shouldBe` "Bad Request"
    it "should send audio" $ do
      Right MessageResponse { message_result = Message { audio = Just Audio { audio_title = Just title } } } <-
        sendAudio token (SendAudioRequest chatId "BQADBAADAQQAAiBOnQHThzc4cz1-IwI" Nothing Nothing Nothing Nothing Nothing) manager
      title `shouldBe` "The Nutcracker Suite - Act II, No.12. Pas de Deux variations"

  describe "/sendSticker" $ do
    it "should send sticker" $ do
      Right MessageResponse { message_result = Message { sticker = Just sticker } } <-
        sendSticker token (SendStickerRequest chatId "BQADAgADGgADkWgMAAGXlYGBiM_d2wI" Nothing Nothing) manager
      (sticker_file_id sticker) `shouldBe` "BQADAgADGgADkWgMAAGXlYGBiM_d2wI"

  describe "/sendLocation" $ do
    it "should send location" $ do
      Right MessageResponse { message_result = Message { location = Just loc } } <-
        sendLocation token (SendLocationRequest chatId 52.38 4.9 Nothing Nothing) manager
      (latitude loc) `shouldSatisfy` (liftM2 (&&) (> 52) (< 52.4))
      (longitude loc) `shouldSatisfy` (liftM2 (&&) (> 4.89) (< 5))

  describe "/sendVenue" $ do
    it "should send a venue" $ do
      Right MessageResponse { message_result = Message { location = Just loc } } <-
        sendVenue token (SendVenueRequest chatId 52.38 4.9 "Amsterdam Centraal" "Amsterdam" Nothing Nothing Nothing) manager
      (latitude loc) `shouldSatisfy` (liftM2 (&&) (> 52) (< 52.4))
      (longitude loc) `shouldSatisfy` (liftM2 (&&) (> 4.89) (< 5))

  describe "/sendContact" $ do
    it "should send a contact" $ do
      Right MessageResponse { message_result = Message { contact = Just con } } <-
        sendContact token (SendContactRequest chatId "06-18035176" "Hilbert" Nothing Nothing Nothing) manager
      -- Telegram seems to remove any non numeric characters from the sent phone number (at least it removed my '-')
      (contact_phone_number con) `shouldBe` "0618035176"
      (contact_first_name con) `shouldBe` "Hilbert"

  describe "/sendChatAction" $ do
    it "should set typing action" $ do
      Right ChatActionResponse { action_result = res} <-
        sendChatAction token (SendChatActionRequest chatId Typing) manager
      res `shouldBe` True
    it "should set find location action" $ do
      Right ChatActionResponse { action_result = res} <-
        sendChatAction token (SendChatActionRequest chatId FindLocation) manager
      res `shouldBe` True
    it "should set upload photo action" $ do
      Right ChatActionResponse { action_result = res} <-
        sendChatAction token (SendChatActionRequest chatId UploadPhoto) manager
      res `shouldBe` True

  describe "/getUpdates" $ do
    it "should get all messages" $ do
      Right UpdatesResponse { update_result = updates} <-
        getUpdates token Nothing Nothing Nothing manager
      (length updates) `shouldSatisfy` (>= 0)

  describe "/getFile" $ do
    it "should get file" $ do
      Right FileResponse { file_result = file } <-
        getFile token "AAQEABMXDZEwAARC0Kj3twkzNcMkAAIC" manager
      (fmap (T.take 10) (file_path file)) `shouldBe` (Just "thumb/file")

    it "should return error" $ do
      Left FailureResponse { responseStatus = Status { statusMessage = msg } } <-
        getFile token "AAQEABMXDZEwAARC0Kj3twkzNcMkAmm" manager
      msg `shouldBe` "Bad Request"

  describe "/getUserProfilePhotos" $ do
   it "should get user profile photos" $ do
     Right UserProfilePhotosResponse { photos_result = photos } <-
       getUserProfilePhotos token (read (T.unpack chatId)) Nothing Nothing manager
     (total_count photos) `shouldSatisfy` (>= 0)

  describe "/setWebhook" $ do
    it "should set webhook" $ do
      Right SetWebhookResponse { webhook_result = res } <-
        setWebhook token (Just "https://example.com/secret_token") manager
      res `shouldBe` True

    it "should remove webhook" $ do
      Right SetWebhookResponse { webhook_result = res } <-
        setWebhook token Nothing manager
      res `shouldBe` True

  describe "/editTextMessage" $ do
    it "should edit message" $ do
      let originalMessage = SendMessageRequest chatId "veritas" Nothing Nothing Nothing Nothing
      Right MessageResponse { message_result = Message { message_id = msg_id, text = Just txt } } <-
        sendMessage token originalMessage manager
      Right MessageResponse { message_result = Message { text = txt' } } <-
        editMessageText token (EditMessageTextRequest (Just chatId) (Just msg_id) Nothing ("edited " <> txt) Nothing Nothing Nothing) manager
      txt' `shouldBe` Just "edited veritas"

    it "should edit caption" $ do
      let originalMessage = SendPhotoRequest chatId catPic (Just "cat picture") Nothing Nothing
      Right MessageResponse { message_result = Message { message_id = msg_id, caption = Just cpt } } <-
        sendPhoto token originalMessage manager
      Right MessageResponse { message_result = Message { caption = Just cpt' } } <-
        editMessageCaption token (EditMessageCaptionRequest (Just chatId) (Just msg_id) Nothing (Just ("edited " <> cpt)) Nothing) manager
      cpt' `shouldBe` "edited cat picture"

    -- it "should edit caption" $ do ... after inline query tests are on place

catPic = "AgADBAADv6cxGybVMgABtZ_EOpBSdxYD5xwZAAS0kQ9gsy1eDh2FAAIC"