{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Web.Telegram.API.Bot.API.Queries
  ( -- * Functions
    answerInlineQuery
  , answerInlineQueryM
  , answerCallbackQuery
  , answerCallbackQueryM
    -- * API
  , TelegramBotQueriesAPI
  , queriesApi
    -- * Types
  ) where

import           Data.Proxy
import           Network.HTTP.Client            (Manager)
import           Servant.API
import           Servant.Client
import           Web.Telegram.API.Bot.API.Core
import           Web.Telegram.API.Bot.Requests
import           Web.Telegram.API.Bot.Responses

-- | Telegram Bot API
type TelegramBotQueriesAPI =
         TelegramToken :> "answerInlineQuery"
         :> ReqBody '[JSON] AnswerInlineQueryRequest
         :> Post '[JSON] InlineQueryResponse
    :<|> TelegramToken :> "answerCallbackQuery"
         :> ReqBody '[JSON] AnswerCallbackQueryRequest
         :> Post '[JSON] CallbackQueryResponse


-- | Proxy for Thelegram Bot API
queriesApi :: Proxy TelegramBotQueriesAPI
queriesApi = Proxy

answerInlineQuery_         :: Token -> AnswerInlineQueryRequest -> ClientM InlineQueryResponse
answerCallbackQuery_       :: Token -> AnswerCallbackQueryRequest -> ClientM CallbackQueryResponse
answerInlineQuery_ :<|> answerCallbackQuery_
     = client queriesApi

-- | Use this method to send answers to an inline query. No more than 50 results per query are allowed.
answerInlineQuery :: Token -> AnswerInlineQueryRequest -> Manager -> IO (Either ClientError InlineQueryResponse)
answerInlineQuery = runM answerInlineQueryM

-- | See 'answerInlineQuery'
answerInlineQueryM :: AnswerInlineQueryRequest -> TelegramClient InlineQueryResponse
answerInlineQueryM = run_ answerInlineQuery_

-- | Use this method to send answers to callback queries sent from inline keyboards. The answer will be displayed to the user as a notification at the top of the chat screen or as an alert.
answerCallbackQuery :: Token -> AnswerCallbackQueryRequest -> Manager -> IO (Either ClientError CallbackQueryResponse)
answerCallbackQuery = runM answerCallbackQueryM

-- | See 'answerCallbackQuery'
answerCallbackQueryM :: AnswerCallbackQueryRequest -> TelegramClient CallbackQueryResponse
answerCallbackQueryM = run_ answerCallbackQuery_
