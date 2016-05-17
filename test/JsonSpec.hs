{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}

module JsonSpec (spec) where

import           Control.Monad
import           Web.Telegram.API.Bot
import           Test.Hspec
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Environment
import           Data.Aeson.Encode
import           Text.JSON.JPath
import           Data.ByteString.Lazy.UTF8

spec :: Spec
spec = do
  let getType = \q -> jPath (T.unpack "type") (toString (encode q))
  describe "type of serialized inline query result" $ do
    it "should be article" $ do
      (getType iq_article) `shouldBe` ["\"article\""]
    it "should be photo" $ do
      (getType iq_photo) `shouldBe` ["\"photo\""]
    it "should be gif" $ do
      (getType iq_gif) `shouldBe` ["\"gif\""]
    it "should be mpeg4_gif" $ do
      (getType iq_mpeg) `shouldBe` ["\"mpeg4_gif\""]
    it "should be video" $ do
      (getType iq_video) `shouldBe` ["\"video\""]
    it "should be audio" $ do
      (getType iq_audio) `shouldBe` ["\"audio\""]
    it "should be contact" $ do
      (getType iq_contact) `shouldBe` ["\"contact\""]
    it "should be document" $ do
      (getType iq_document) `shouldBe` ["\"document\""]
    it "should be location" $ do
      (getType iq_location) `shouldBe` ["\"location\""]
    it "should be venue" $ do
      (getType iq_venue) `shouldBe` ["\"venue\""]
  describe "type of serialized cached inline query result" $ do
    it "should be audio" $ do
      (getType cached_audio) `shouldBe` ["\"audio\""]
    it "should be voice" $ do
      (getType cached_voice) `shouldBe` ["\"voice\""]
    it "should be video" $ do
      (getType cached_video) `shouldBe` ["\"video\""]
    it "should be document" $ do
      (getType cached_doc) `shouldBe` ["\"document\""]
    it "should be sticker" $ do
      (getType cached_sticker) `shouldBe` ["\"sticker\""]
    it "should be mpeg4_gif" $ do
      (getType cached_mpeg) `shouldBe` ["\"mpeg4_gif\""]
    it "should be gif" $ do
      (getType cached_gif) `shouldBe` ["\"gif\""]
    it "should be photo" $ do
      (getType cached_photo) `shouldBe` ["\"photo\""]

message_content = InputTextMessageContent "test message content" Nothing Nothing

iq_article = inlineQueryResultArticle "" "text article content" message_content
iq_photo = inlineQueryResultPhoto "" "" ""
iq_gif = inlineQueryResultGif "" "" ""
iq_mpeg = inlineQueryResultMpeg4Gif "" "" ""
iq_video = inlineQueryResultVideo "" "" "video/mpeg" "" ""
iq_audio = inlineQueryResultAudio "" "" ""
iq_contact = inlineQueryResultContact "" "" ""
iq_document = inlineQueryResultDocument "" "" "" ""
iq_location = inlineQueryResultLocation "" 0.0 0.0 ""
iq_venue = inlineQueryResultVenue "" 0.0 0.0 "" ""
cached_audio = inlineQueryResultCachedAudio "" ""
cached_voice = inlineQueryResultCachedVoice "" "" ""
cached_video = inlineQueryResultCachedVideo "" "" ""
cached_doc = inlineQueryResultCachedDocument "" "" ""
cached_sticker = inlineQueryResultCachedSticker "" ""
cached_mpeg = inlineQueryResultCachedMpeg4Gif "" ""
cached_gif = inlineQueryResultCachedGif "" ""
cached_photo = inlineQueryResultCachedPhoto "" ""
