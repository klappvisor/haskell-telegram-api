{-# LANGUAGE OverloadedStrings #-}

module InlineSpec (spec) where

import           Control.Monad
import           Web.Telegram.API.Bot
import           Test.Hspec
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Servant.Client
import           Servant.API
import           Network.HTTP.Types.Status
import           System.Environment

spec :: Token -> Text -> Spec
spec token chatId = do
  let inline_query_id = ""
  manager <- runIO $ newManager tlsManagerSettings
  -- Create the tls connection manager
  describe "/answerInlineQuery" $ do
    it "should answer with article" $ do
      Right InlineQueryResponse { query_result = res } <-
        answerInlineQuery token (AnswerInlineQueryRequest inline_query_id [inline_article] Nothing Nothing Nothing Nothing Nothing) manager
      res `shouldBe` True
    it "should answer with photo" $ do
      Right InlineQueryResponse { query_result = res } <-
        answerInlineQuery token (AnswerInlineQueryRequest inline_query_id [inline_photo] Nothing Nothing Nothing Nothing Nothing) manager
      res `shouldBe` True
    it "should answer with gif" $ do
      Right InlineQueryResponse { query_result = res } <-
        answerInlineQuery token (AnswerInlineQueryRequest inline_query_id [inline_gif] Nothing Nothing Nothing Nothing Nothing) manager
      res `shouldBe` True
    it "should answer with mpeg gif" $ do
      Right InlineQueryResponse { query_result = res } <-
        answerInlineQuery token (AnswerInlineQueryRequest inline_query_id [inline_mpeg] Nothing Nothing Nothing Nothing Nothing) manager
      res `shouldBe` True
    it "should answer with video" $ do
      Right InlineQueryResponse { query_result = res } <-
        answerInlineQuery token (AnswerInlineQueryRequest inline_query_id [inline_video] Nothing Nothing Nothing Nothing Nothing) manager
      res `shouldBe` True

  describe "/answerInlineQuery" $ do
    it "should get updates and answer" $ do
      Right UpdatesResponse { update_result = updates} <-
        getUpdates token Nothing Nothing Nothing manager
      Update { inline_query = Just (InlineQuery { query_id = id } ) } <- pure (last updates)
      e <-
        answerInlineQuery token (AnswerInlineQueryRequest id [inline_video] Nothing Nothing Nothing Nothing Nothing) manager
      putStrLn (show e)

message_content = InputTextMessageContent "test message content" Nothing Nothing

inline_article = InlineQueryResultArticle "2131341" (Just "text article content") (Just message_content) Nothing Nothing Nothing Nothing Nothing Nothing Nothing
inline_photo = InlineQueryResultPhoto "1430810" "http://vignette3.wikia.nocookie.net/victorious/images/f/f8/NyanCat.jpg" (Just "http://vignette3.wikia.nocookie.net/victorious/images/f/f8/NyanCat.jpg") Nothing Nothing Nothing Nothing Nothing Nothing Nothing
inline_gif = InlineQueryResultGif "131231234" "https://media.giphy.com/media/zEO5eq3ZsEwbS/giphy.gif" Nothing Nothing (Just "https://media.giphy.com/media/zEO5eq3ZsEwbS/100.gif") Nothing Nothing Nothing Nothing
inline_mpeg = InlineQueryResultMpeg4Gif "131251234" "https://media.giphy.com/media/zEO5eq3ZsEwbS/giphy.gif" Nothing Nothing (Just "https://media.giphy.com/media/zEO5eq3ZsEwbS/100.gif") Nothing Nothing Nothing Nothing
inline_video = InlineQueryResultVideo "123413542" "https://www.youtube.com/embed/TBKN7_vx2xo" "text/html" (Just "https://i.ytimg.com/vi_webp/TBKN7_vx2xo/mqdefault.webp") (Just "Enjoykin — Nyash Myash") Nothing Nothing Nothing Nothing Nothing Nothing Nothing
