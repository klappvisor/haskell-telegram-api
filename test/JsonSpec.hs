{-# LANGUAGE OverloadedStrings #-}

module JsonSpec (spec) where

import           Web.Telegram.API.Bot
import           Test.Hspec
import qualified Data.Text as T
import           Data.Aeson
import           Text.JSON.JPath
import           Data.ByteString.Lazy.UTF8

spec :: Spec
spec = do
  let getType q = jPath (T.unpack "type") (toString (encode q))
  describe "type of serialized inline query result" $ do
    it "should be article" $
      getType iq_article `shouldBe` ["\"article\""]
    it "should be photo" $
      getType iq_photo `shouldBe` ["\"photo\""]
    it "should be gif" $
      getType iq_gif `shouldBe` ["\"gif\""]
    it "should be mpeg4_gif" $
      getType iq_mpeg `shouldBe` ["\"mpeg4_gif\""]
    it "should be video" $
      getType iq_video `shouldBe` ["\"video\""]
    it "should be audio" $
      getType iq_audio `shouldBe` ["\"audio\""]
    it "should be contact" $
      getType iq_contact `shouldBe` ["\"contact\""]
    it "should be document" $
      getType iq_document `shouldBe` ["\"document\""]
    it "should be location" $
      getType iq_location `shouldBe` ["\"location\""]
    it "should be venue" $
      getType iq_venue `shouldBe` ["\"venue\""]
  describe "type of serialized cached inline query result" $ do
    it "should be audio" $
      getType cached_audio `shouldBe` ["\"audio\""]
    it "should be voice" $
      getType cached_voice `shouldBe` ["\"voice\""]
    it "should be video" $
      getType cached_video `shouldBe` ["\"video\""]
    it "should be document" $
      getType cached_doc `shouldBe` ["\"document\""]
    it "should be sticker" $
      getType cached_sticker `shouldBe` ["\"sticker\""]
    it "should be mpeg4_gif" $
      getType cached_mpeg `shouldBe` ["\"mpeg4_gif\""]
    it "should be gif" $
      getType cached_gif `shouldBe` ["\"gif\""]
    it "should be photo" $
      getType cached_photo `shouldBe` ["\"photo\""]

message_content :: InputMessageContent
message_content = InputTextMessageContent "test message content" Nothing Nothing

iq_article :: InlineQueryResult
iq_article = inlineQueryResultArticle "" "text article content" message_content

iq_photo :: InlineQueryResult
iq_photo = inlineQueryResultPhoto "" "" ""

iq_gif :: InlineQueryResult
iq_gif = inlineQueryResultGif "" "" ""

iq_mpeg :: InlineQueryResult
iq_mpeg = inlineQueryResultMpeg4Gif "" "" ""

iq_video :: InlineQueryResult
iq_video = inlineQueryResultVideo "" "" "video/mpeg" "" ""

iq_audio :: InlineQueryResult
iq_audio = inlineQueryResultAudio "" "" ""

iq_contact :: InlineQueryResult
iq_contact = inlineQueryResultContact "" "" ""

iq_document :: InlineQueryResult
iq_document = inlineQueryResultDocument "" "" "" ""

iq_location :: InlineQueryResult
iq_location = inlineQueryResultLocation "" 0.0 0.0 ""

iq_venue :: InlineQueryResult
iq_venue = inlineQueryResultVenue "" 0.0 0.0 "" ""

cached_audio :: InlineQueryResult
cached_audio = inlineQueryResultCachedAudio "" ""

cached_voice :: InlineQueryResult
cached_voice = inlineQueryResultCachedVoice "" "" ""

cached_video :: InlineQueryResult
cached_video = inlineQueryResultCachedVideo "" "" ""

cached_doc :: InlineQueryResult
cached_doc = inlineQueryResultCachedDocument "" "" ""

cached_sticker :: InlineQueryResult
cached_sticker = inlineQueryResultCachedSticker "" ""

cached_mpeg :: InlineQueryResult
cached_mpeg = inlineQueryResultCachedMpeg4Gif "" ""

cached_gif :: InlineQueryResult
cached_gif = inlineQueryResultCachedGif "" ""

cached_photo :: InlineQueryResult
cached_photo = inlineQueryResultCachedPhoto "" ""
