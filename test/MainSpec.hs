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
import           System.FilePath
import           Control.Concurrent

import           Paths_telegram_api

-- to print out remote response if response success not match
success, nosuccess :: (Show a, Show b) =>Either a b ->Expectation
success   e = e `shouldSatisfy` isRight
nosuccess e = e `shouldSatisfy` isLeft

spec :: Token -> Text -> Text -> Spec
spec token chatId botName = do
  manager <- runIO $ newManager tlsManagerSettings
  dataDir <- runIO getDataDir
  let testFile name = dataDir </> "test-data" </> name
  describe "/getMe" $
    it "responds with correct bot's name" $ do
      Right Response { result = u } <-
        getMe token manager
      user_first_name u `shouldBe` botName -- f.e. "TelegramAPIBot"

  describe "/sendMessage" $ do
    it "should send message" $ do
      res <- sendMessage token (sendMessageRequest chatId "test message") manager
      success res
      let Right Response { result = m } = res
      text m `shouldBe` Just "test message"

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
      let Right Response { result = m } = res
      text m `shouldBe` Just "text bold italic github"

    it "should set keyboard" $ do
      let kbA = keyboardButton "A"
          kbB = keyboardButton "B"
          kbC = keyboardButton "C"
      let message = (sendMessageRequest chatId "set keyboard") {
        message_reply_markup = Just $ replyKeyboardMarkup [[kbA, kbB, kbC]]
      }
      res <- sendMessage token message manager
      success res
      let Right Response { result = m } = res
      text m `shouldBe` Just "set keyboard"

    it "should remove keyboard" $ do
      let message = (sendMessageRequest chatId "remove keyboard") {
        message_reply_markup = Just replyKeyboardHide
      }
      res <- sendMessage token message manager
      success res
      let Right Response { result = m } = res
      text m `shouldBe` Just "remove keyboard"

    it "should send message with inline keyboard" $ do
      let kbA = (inlineKeyboardButton "A") { ikb_callback_data = Just "A" }
          kbB = (inlineKeyboardButton "B") { ikb_callback_data = Just "B" }
          kbC = (inlineKeyboardButton "C") { ikb_callback_data = Just "C" }
      let message = (sendMessageRequest chatId "set inline keyboard") {
        message_reply_markup = Just $ inlineKeyboardMarkup [[kbA, kbB, kbC]]
      }
      res <- sendMessage token message manager
      success res
      let Right Response { result = m } = res
      text m `shouldBe` Just "set inline keyboard"

    it "should force reply" $ do
      let message = (sendMessageRequest chatId "force reply") {
        message_reply_markup = Just forceReply
      }
      res <- sendMessage token message manager
      success res
      let Right Response { result = m } = res
      text m `shouldBe` Just "force reply"

  describe "/forwardMessage" $
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
      Left FailureResponse { responseStatus = Status { statusMessage = msg } } <- sendPhoto token photo manager
      msg `shouldBe` "Bad Request"
    it "should upload photo and resend it by id" $ do
      let fileUpload = localFileUpload (testFile "christmas-cat.jpg")
      let upload = (uploadPhotoRequest chatId fileUpload) {
        photo_caption = Just "uploaded photo"
      }
      Right Response { result = Message { caption = Just cpt, photo = Just photos } } <-
        uploadPhoto token upload manager
      cpt `shouldBe` "uploaded photo"
      -- resend by id
      let id = (photo_file_id . last) photos
      let photo = (sendPhotoRequest chatId id) {
        photo_caption = Just "photo caption"
      }
      Right Response { result = Message { caption = Just cpt } } <-
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
    it "should upload audio and resend it by id" $ do
      let fileUpload = localFileUpload (testFile "concerto-for-2-trumpets-in-c-major.mp3")
          audioTitle = "Concerto for 2 Trumpets in C major, RV. 537 (Rondeau arr.) All."
          audioPerformer = "Michel Rondeau"
          audio = (uploadAudioRequest chatId fileUpload) {
            _audio_performer = Just audioPerformer,
            _audio_title = Just audioTitle
          }
      Right Response {
        result = Message {
          audio = Just Audio {
            audio_file_id = file_id, audio_title = Just title, audio_performer = Just performer
          }
        }
      } <- uploadAudio token audio manager
      title `shouldBe` audioTitle
      performer `shouldBe` audioPerformer
      let audio = sendAudioRequest chatId file_id
      Right Response { result = Message { audio = Just Audio { audio_title = Just title' } } } <-
        sendAudio token audio manager
      title' `shouldBe` audioTitle

  describe "/sendSticker" $ do
    it "should send sticker" $ do
      let sticker = sendStickerRequest chatId "BQADAgADGgADkWgMAAGXlYGBiM_d2wI"
      Right Response { result = Message { sticker = Just sticker } } <-
        sendSticker token sticker manager
      sticker_file_id sticker `shouldBe` "BQADAgADGgADkWgMAAGXlYGBiM_d2wI"
    it "should upload sticker" $ do
      let fileUpload = localFileUpload (testFile "haskell-logo.webp")
          stickerReq = uploadStickerRequest chatId fileUpload
      Right Response { result = Message { sticker = Just sticker } } <-
        uploadSticker token stickerReq manager
      sticker_height sticker `shouldBe` 128

  describe "/sendVoice" $
    it "should upload voice" $ do
      -- audio source: https://commons.wikimedia.org/wiki/File:Possible_PDM_signal_labeled_as_Sputnik_by_NASA.ogg
      let fileUpload = localFileUpload (testFile "Possible_PDM_signal_labeled_as_Sputnik_by_NASA.ogg")
          voiceReq = (uploadVoiceRequest chatId fileUpload) { _voice_duration = Just 10 }
      Right Response { result = Message { voice = Just voice } } <-
        uploadVoice token voiceReq manager
      voice_duration voice `shouldBe` 10

  describe "/sendVideo" $
    it "should upload video" $ do
      -- video source: http://techslides.com/sample-webm-ogg-and-mp4-video-files-for-html5
      let fileUpload = localFileUpload (testFile "lego-video.mp4")
          videoReq = uploadVideoRequest chatId fileUpload
      Right Response { result = Message { video = Just video } } <-
        uploadVideo token videoReq manager
      video_width video `shouldBe` 560

  describe "/sendDocument" $
    it "should upload document" $ do
      let fileUpload = localFileUpload (testFile "wikipedia-telegram.txt")
          documentReq = uploadDocumentRequest chatId fileUpload
      Right Response { result = Message { document = Just document } } <-
        uploadDocument token documentReq manager
      doc_mime_type document `shouldBe` Just "text/plain"
      doc_file_name document `shouldBe` Just "wikipedia-telegram.txt"

  describe "/sendLocation" $
    it "should send location" $ do
      let location = sendLocationRequest chatId 52.38 4.9
      Right Response { result = Message { location = Just loc } } <-
        sendLocation token location manager
      latitude loc `shouldSatisfy` liftM2 (&&) (> 52) (< 52.4)
      longitude loc `shouldSatisfy` liftM2 (&&) (> 4.89) (< 5)

  describe "/sendVenue" $
    it "should send a venue" $ do
      let venue = sendVenueRequest chatId 52.38 4.9 "Amsterdam Centraal" "Amsterdam"
      Right Response { result = Message { location = Just loc } } <-
        sendVenue token venue manager
      latitude loc `shouldSatisfy` liftM2 (&&) (> 52) (< 52.4)
      longitude loc `shouldSatisfy` liftM2 (&&) (> 4.89) (< 5)

  describe "/sendContact" $
    it "should send a contact" $ do
      let contact = sendContactRequest chatId "06-18035176" "Hilbert"
      Right Response { result = Message { contact = Just con } } <-
        sendContact token contact manager
      -- Telegram seems to remove any non numeric characters from the sent phone number (at least it removed my '-')
      contact_phone_number con `shouldBe` "0618035176"
      contact_first_name con `shouldBe` "Hilbert"

  describe "/sendChatAction" $ do
    it "should set typing action" $ do
      Right Response { result = res} <-
        sendChatAction token (SendChatActionRequest chatId Typing) manager
      res `shouldBe` True
    it "should set find location action" $ do
      Right Response { result = res} <-
        sendChatAction token (SendChatActionRequest chatId FindLocation) manager
      res `shouldBe` True
    it "should set upload photo action" $ do
      Right Response { result = res} <-
        sendChatAction token (SendChatActionRequest chatId UploadPhoto) manager
      res `shouldBe` True

  describe "/getUpdates" $
    it "should get all messages" $ do
      Right Response { result = updates} <-
        getUpdates token Nothing Nothing Nothing manager
      length updates `shouldSatisfy` (>= 0)

  describe "/getFile" $ do
    it "should get file" $ do
      Right Response { result = file } <-
        getFile token "AAQEABMXDZEwAARC0Kj3twkzNcMkAAIC" manager
      fmap (T.take 10) (file_path file) `shouldBe` Just "thumb/file"

    it "should return error" $ do
      Left FailureResponse { responseStatus = Status { statusMessage = msg } } <-
        getFile token "AAQEABMXDZEwAARC0Kj3twkzNcMkAmm" manager
      msg `shouldBe` "Bad Request"

  describe "/getUserProfilePhotos" $
   it "should get user profile photos" $ do
     Right Response { result = photos } <-
       getUserProfilePhotos token (read (T.unpack chatId)) Nothing Nothing manager
     total_count photos `shouldSatisfy` (>= 0)

  describe "/setWebhook and /getWebhookInfo" $ do
    it "should set webhook with certificate" $ do
      let cert = localFileUpload $ testFile "cert.pem"
          req = SetWebhookRequest "https://example.com/secret_token" cert
      res <- setWebhookWithCertificate token req manager
      success res
      let Right Response { result = val } = res
      val `shouldBe` True

    it "should set webhook" $ do
      threadDelay $ 2 * 1000 * 1000
      Right Response { result = res } <-
        setWebhook token (Just "https://example.com/secret_token") manager
      res `shouldBe` True

    it "should get webhook info" $ do
      Right Response { result = WebhookInfo { whi_url = url } } <- getWebhookInfo token manager
      url `shouldBe` "https://example.com/secret_token"

    it "should remove webhook" $ do
      Right Response { result = res } <-
        setWebhook token Nothing manager
      res `shouldBe` True

    it "should remove webhood with deleteWebhook" $ do
      threadDelay $ 2 * 1000 * 1000
      _ <- setWebhook token (Just "https://example.com/secret_token") manager
      res <- deleteWebhook token manager
      success res
      let Right Response { result = val } = res
      val `shouldBe` True

  describe "/editTextMessage" $ do
    it "should edit message" $ do
      let originalMessage = sendMessageRequest chatId "veritas"
      Right Response { result = Message { message_id = msg_id, text = Just txt } } <-
        sendMessage token originalMessage manager
      let editRequest = editMessageTextRequest chatId msg_id $ "edited " <> txt
      Right Response { result = Message { text = txt' } } <-
        editMessageText token editRequest manager
      txt' `shouldBe` Just "edited veritas"

    it "should edit caption" $ do
      let fileUpload = localFileUpload (testFile "christmas-cat.jpg")
      let originalMessage = (uploadPhotoRequest chatId fileUpload) {
        photo_caption = Just "cat picture"
      }
      Right Response { result = Message { message_id = msg_id, caption = Just cpt } } <-
        uploadPhoto token originalMessage manager
      let editRequest = editMessageCaptionRequest chatId msg_id $ Just $ "edited " <> cpt
      Right Response { result = Message { caption = Just cpt' } } <-
        editMessageCaption token editRequest manager
      cpt' `shouldBe` "edited cat picture"

    -- it "should edit caption" $ do ... after inline query tests are on place
