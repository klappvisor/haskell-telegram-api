{-# LANGUAGE OverloadedStrings #-}

module InlineSpec (spec) where

import           Web.Telegram.API.Bot
import           Test.Hspec
import           Data.Text (Text)
import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)

spec :: Token -> Text -> Spec
spec token chatId = do
  let inline_query_id = ""
  manager <- runIO $ newManager tlsManagerSettings
  -- Create the tls connection manager
  describe "/answerInlineQuery" $ do
    it "should answer with article" $ do
      Right Response { result = res } <-
        answerInlineQuery token (answerInlineQueryRequest inline_query_id [inline_article]) manager
      res `shouldBe` True
    it "should answer with photo" $ do
      Right Response { result = res } <-
        answerInlineQuery token (answerInlineQueryRequest inline_query_id [inline_photo]) manager
      res `shouldBe` True
    it "should answer with gif" $ do
      Right Response { result = res } <-
        answerInlineQuery token (answerInlineQueryRequest inline_query_id [inline_gif]) manager
      res `shouldBe` True
    it "should answer with mpeg gif" $ do
      Right Response { result = res } <-
        answerInlineQuery token (answerInlineQueryRequest inline_query_id [inline_mpeg]) manager
      res `shouldBe` True
    it "should answer with video" $ do
      Right Response { result = res } <-
        answerInlineQuery token (answerInlineQueryRequest inline_query_id [inline_video]) manager
      res `shouldBe` True

  describe "/answerInlineQuery" $
    it "should get updates and answer" $ do
      Right Response { result = updates} <-
        getUpdates token Nothing Nothing Nothing manager
      Update { inline_query = Just (InlineQuery { query_id = id } ) } <- pure (last updates)
      e <-
        answerInlineQuery token (answerInlineQueryRequest id [inline_video]) manager
      putStrLn (show e)

message_content = InputTextMessageContent "test message content" Nothing Nothing

inline_article = InlineQueryResultArticle "2131341" (Just "text article content") (Just message_content) Nothing Nothing Nothing Nothing Nothing Nothing Nothing
inline_photo = InlineQueryResultPhoto "1430810" "http://vignette3.wikia.nocookie.net/victorious/images/f/f8/NyanCat.jpg" (Just "http://vignette3.wikia.nocookie.net/victorious/images/f/f8/NyanCat.jpg") Nothing Nothing Nothing Nothing Nothing Nothing Nothing
inline_gif = InlineQueryResultGif "131231234" "https://media.giphy.com/media/zEO5eq3ZsEwbS/giphy.gif" Nothing Nothing (Just "https://media.giphy.com/media/zEO5eq3ZsEwbS/100.gif") Nothing Nothing Nothing Nothing
inline_mpeg = InlineQueryResultMpeg4Gif "131251234" "https://media.giphy.com/media/zEO5eq3ZsEwbS/giphy.gif" Nothing Nothing (Just "https://media.giphy.com/media/zEO5eq3ZsEwbS/100.gif") Nothing Nothing Nothing Nothing
inline_video = InlineQueryResultVideo "123413542" "https://www.youtube.com/embed/TBKN7_vx2xo" "text/html" (Just "https://i.ytimg.com/vi_webp/TBKN7_vx2xo/mqdefault.webp") (Just "Enjoykin — Nyash Myash") Nothing Nothing Nothing Nothing Nothing Nothing Nothing
