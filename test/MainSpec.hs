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
      res <- sendMessage token (sendMessageRequest chatId "test message") manager
      success res 
      let Right MessageResponse { message_result = m } = res
      (text m) `shouldBe` (Just "test message")

    it "should return error message" $ do
      res <- sendMessage token (sendMessageRequest "" "test message") manager
      nosuccess res
      let Left FailureResponse { responseStatus = Status { statusMessage = msg } } = res
      msg `shouldBe` "Bad Request"

    it "should send message markdown" $ do
      let request = (sendMessageRequest chatId "text *bold* _italic_ [github](github.com/klappvisor/telegram-api)") {
          message_parse_mode = Just Markdown
        }
      res <- sendMessage token request manager
      success res
      let Right MessageResponse { message_result = m } = res
      (text m) `shouldBe` (Just "text bold italic github")

    it "should set keyboard" $ do
      let kbA = keyboardButton "A"
          kbB = keyboardButton "B"
          kbC = keyboardButton "C"
      let message = (sendMessageRequest chatId "set keyboard") {
        message_reply_markup = Just $ replyKeyboardMarkup [[kbA, kbB, kbC]]
      }
      res <- sendMessage token message manager
      success res
      let Right MessageResponse { message_result = m } = res
      (text m) `shouldBe` (Just "set keyboard")

    it "should remove keyboard" $ do
      let message = (sendMessageRequest chatId "remove keyboard") {
        message_reply_markup = Just replyKeyboardHide
      }
      res <- sendMessage token message manager
      success res
      let Right MessageResponse { message_result = m } = res
      (text m) `shouldBe` (Just "remove keyboard")

    it "should force reply" $ do
      let message = (sendMessageRequest chatId "force reply") {
        message_reply_markup = Just forceReply
      }
      res <- sendMessage token message manager
      success res
      let Right MessageResponse { message_result = m } = res
      (text m) `shouldBe` (Just "force reply")

  describe "/forwardMessage" $ do
    it "should forward message" $ do
      res <- forwardMessage token (forwardMessageRequest chatId chatId 123000) manager
      nosuccess res
      let Left FailureResponse { responseStatus = Status { statusMessage = msg } } = res
      msg `shouldBe` "Bad Request"

  describe "/sendPhoto" $ do
    it "should return error message" $ do
      let photo = (sendPhotoRequest "" "photo_id") {
        photo_caption = Just "photo caption"
      }
      Left FailureResponse { responseStatus = Status { statusMessage = msg } } <-
        sendPhoto token photo manager
      msg `shouldBe` "Bad Request"
    it "should send photo" $ do
      let photo = (sendPhotoRequest chatId catPic) {
        photo_caption = Just "photo caption"
      }
      Right MessageResponse { message_result = Message { caption = Just cpt } } <-
        sendPhoto token photo manager
      cpt `shouldBe` "photo caption"

  describe "/sendAudio" $ do
    it "should return error message" $ do
      let audio = (sendAudioRequest "" "audio_id") {
        _audio_performer = Just "performer"
      , _audio_title = Just "title"
      }
      Left FailureResponse { responseStatus = Status { statusMessage = msg } } <-
        sendAudio token audio manager
      msg `shouldBe` "Bad Request"
    it "should send audio" $ do
      let audio = sendAudioRequest chatId "BQADBAADAQQAAiBOnQHThzc4cz1-IwI"
      Right MessageResponse { message_result = Message { audio = Just Audio { audio_title = Just title } } } <-
        sendAudio token audio manager
      title `shouldBe` "The Nutcracker Suite - Act II, No.12. Pas de Deux variations"

  describe "/sendSticker" $ do
    it "should send sticker" $ do
      let sticker = sendStickerRequest chatId "BQADAgADGgADkWgMAAGXlYGBiM_d2wI"
      Right MessageResponse { message_result = Message { sticker = Just sticker } } <-
        sendSticker token sticker manager
      (sticker_file_id sticker) `shouldBe` "BQADAgADGgADkWgMAAGXlYGBiM_d2wI"

  describe "/sendLocation" $ do
    it "should send location" $ do
      let location = sendLocationRequest chatId 52.38 4.9
      Right MessageResponse { message_result = Message { location = Just loc } } <-
        sendLocation token location manager
      (latitude loc) `shouldSatisfy` (liftM2 (&&) (> 52) (< 52.4))
      (longitude loc) `shouldSatisfy` (liftM2 (&&) (> 4.89) (< 5))

  describe "/sendVenue" $ do
    it "should send a venue" $ do
      let venue = sendVenueRequest chatId 52.38 4.9 "Amsterdam Centraal" "Amsterdam"
      Right MessageResponse { message_result = Message { location = Just loc } } <-
        sendVenue token venue manager
      (latitude loc) `shouldSatisfy` (liftM2 (&&) (> 52) (< 52.4))
      (longitude loc) `shouldSatisfy` (liftM2 (&&) (> 4.89) (< 5))

  describe "/sendContact" $ do
    it "should send a contact" $ do
      let contact = sendContactRequest chatId "06-18035176" "Hilbert"
      Right MessageResponse { message_result = Message { contact = Just con } } <-
        sendContact token contact manager
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
      let originalMessage = sendMessageRequest chatId "veritas"
      Right MessageResponse { message_result = Message { message_id = msg_id, text = Just txt } } <-
        sendMessage token originalMessage manager
      let editRequest = editMessageTextRequest chatId msg_id $ "edited " <> txt
      Right MessageResponse { message_result = Message { text = txt' } } <-
        editMessageText token editRequest manager
      txt' `shouldBe` Just "edited veritas"

    it "should edit caption" $ do
      let originalMessage = (sendPhotoRequest chatId catPic) {
        photo_caption = Just "cat picture"
      }
      Right MessageResponse { message_result = Message { message_id = msg_id, caption = Just cpt } } <-
        sendPhoto token originalMessage manager
      let editRequest = editMessageCaptionRequest chatId msg_id $ Just $ "edited " <> cpt
      Right MessageResponse { message_result = Message { caption = Just cpt' } } <-
        editMessageCaption token editRequest manager
      cpt' `shouldBe` "edited cat picture"

    -- it "should edit caption" $ do ... after inline query tests are on place

catPic = "AgADBAADv6cxGybVMgABtZ_EOpBSdxYD5xwZAAS0kQ9gsy1eDh2FAAIC"
