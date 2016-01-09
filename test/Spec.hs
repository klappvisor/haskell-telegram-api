{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where

import           Control.Monad
import           Web.Telegram.API.Bot
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
        sendMessage token (SendMessageRequest chatId "test message" Nothing Nothing Nothing Nothing)
      (text m) `shouldBe` (Just "test message")

    it "should return error message" $ do
      Left FailureResponse { responseStatus = Status { statusMessage = msg } } <-
        sendMessage token (SendMessageRequest "" "test message" Nothing Nothing Nothing Nothing)
      msg `shouldBe` "Bad Request"

    it "should send message markdown" $ do
      Right MessageResponse { message_result = m } <-
        sendMessage token (SendMessageRequest chatId "text *bold* _italic_ [github](github.com/klappvisor/telegram-api)" (Just Markdown) Nothing Nothing Nothing)
      (text m) `shouldBe` (Just "text bold italic github")

    it "should set keyboard" $ do
      Right MessageResponse { message_result = m } <-
        sendMessage token (SendMessageRequest chatId "set keyboard" Nothing Nothing Nothing (Just (ReplyKeyboardMarkup [["A", "B"], ["C"]] Nothing Nothing Nothing)))
      (text m) `shouldBe` (Just "set keyboard")

    it "should remove keyboard" $ do
      Right MessageResponse { message_result = m } <-
        sendMessage token (SendMessageRequest chatId "remove keyboard" Nothing Nothing Nothing (Just (ReplyKeyboardHide True Nothing)))
      (text m) `shouldBe` (Just "remove keyboard")

    it "should force reply" $ do
      Right MessageResponse { message_result = m } <-
        sendMessage token (SendMessageRequest chatId "force reply" Nothing Nothing Nothing (Just (ForceReply True Nothing)))
      (text m) `shouldBe` (Just "force reply")

  describe "/forwardMessage" $ do
    it "should forward message" $ do
      Left FailureResponse { responseStatus = Status { statusMessage = msg } } <-
        forwardMessage token (ForwardMessageRequest chatId chatId 123)
      msg `shouldBe` "Bad Request"

  describe "/sendPhoto" $ do
    it "should return error message" $ do
      Left FailureResponse { responseStatus = Status { statusMessage = msg } } <-
        sendPhoto token (SendPhotoRequest "" "photo_id" (Just "photo caption") Nothing Nothing)
      msg `shouldBe` "Bad Request"
    it "should send photo" $ do
      Right MessageResponse { message_result = Message { caption = Just cpt } } <-
        sendPhoto token (SendPhotoRequest chatId "AgADBAADv6cxGybVMgABtZ_EOpBSdxYD5xwZAAQ4ElUVMAsbbBqFAAIC" (Just "photo caption") Nothing Nothing)
      cpt `shouldBe` "photo caption"

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

--  describe "/answerInlineQuery" $ do
--    it "should answer with article" $ do
--      Right InlineQueryResponse { query_result = res } <-
--        answerInlineQuery token (AnswerInlineQueryRequest inline_query_id [inline_article] Nothing Nothing Nothing)
--      res `shouldBe` True
--    it "should answer with photo" $ do
--      --Right InlineQueryResponse { query_result = res } <-
--      e <-
--        answerInlineQuery token (AnswerInlineQueryRequest inline_query_id [inline_photo] Nothing Nothing Nothing)
--      --res `shouldBe` True
--      putStrLn (show e)
--    it "should answer with gif" $ do
--      --Right InlineQueryResponse { query_result = res } <-
--      e <-
--        answerInlineQuery token (AnswerInlineQueryRequest inline_query_id [inline_gif] Nothing Nothing Nothing)
--      --res `shouldBe` True
--      putStrLn (show e)
--    it "should answer with mpeg gif" $ do
--      --Right InlineQueryResponse { query_result = res } <-
--      e <-
--        answerInlineQuery token (AnswerInlineQueryRequest inline_query_id [inline_mpeg] Nothing Nothing Nothing)
--      --res `shouldBe` True
--      putStrLn (show e)
--    it "should answer with video" $ do
--      --Right InlineQueryResponse { query_result = res } <-
--      e <-
--        answerInlineQuery token (AnswerInlineQueryRequest inline_query_id [inline_video] Nothing Nothing Nothing)
--      --res `shouldBe` True
--      putStrLn (show e)
--
--  describe "/answerInlineQuery" $ do
--    it "should get updates and answer" $ do
--      Right UpdatesResponse { update_result = updates} <-
--        getUpdates token Nothing Nothing Nothing
--      Update { inline_query = Just (InlineQuery { query_id = id } ) } <- pure (last updates)
--      e <-
--        answerInlineQuery token (AnswerInlineQueryRequest id [inline_video] Nothing Nothing Nothing)
--      putStrLn (show e)

--inline_article = InlineQueryResultArticle "2131341" (Just "text article content") Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
--inline_photo = InlineQueryResultPhoto "1430810" "http://vignette3.wikia.nocookie.net/victorious/images/f/f8/NyanCat.jpg" Nothing Nothing (Just "http://vignette3.wikia.nocookie.net/victorious/images/f/f8/NyanCat.jpg") Nothing Nothing Nothing Nothing Nothing Nothing
--inline_gif = InlineQueryResultGif "131231234" "https://media.giphy.com/media/zEO5eq3ZsEwbS/giphy.gif" Nothing Nothing (Just "https://media.giphy.com/media/zEO5eq3ZsEwbS/100.gif") Nothing Nothing Nothing Nothing Nothing
--inline_mpeg = InlineQueryResultMpeg4Gif "131251234" "https://media.giphy.com/media/zEO5eq3ZsEwbS/giphy.gif" Nothing Nothing (Just "https://media.giphy.com/media/zEO5eq3ZsEwbS/100.gif") Nothing Nothing Nothing Nothing Nothing
--inline_video = InlineQueryResultVideo "123413542" "https://www.youtube.com/embed/TBKN7_vx2xo" "text/html" (Just "Enjoykin — Nyash Myash") Nothing Nothing Nothing Nothing Nothing (Just "https://i.ytimg.com/vi_webp/TBKN7_vx2xo/mqdefault.webp") (Just "Enjoykin — Nyash Myash") Nothing