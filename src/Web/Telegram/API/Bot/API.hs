{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Web.Telegram.API.Bot.API
  ( -- * Functions
    module API
  , runClient
  , runClient'
  , runTelegramClient
    -- * API
  , TelegramBotAPI
  , api
    -- * Types
  , Token             (..)
  , TelegramClient
  ) where

import           Data.Proxy
import           Servant.API
import           Web.Telegram.API.Bot.API.Chats    as API
import           Web.Telegram.API.Bot.API.Core
import           Web.Telegram.API.Bot.API.Edit     as API
import           Web.Telegram.API.Bot.API.Get      as API
import           Web.Telegram.API.Bot.API.Messages as API
import           Web.Telegram.API.Bot.API.Payments as API
import           Web.Telegram.API.Bot.API.Queries  as API
import           Web.Telegram.API.Bot.API.Stickers as API
import           Web.Telegram.API.Bot.API.Updates  as API

type TelegramBotAPI =
       TelegramBotMessagesAPI
  :<|> TelegramBotUpdatesAPI
  :<|> TelegramBotChatsAPI
  :<|> TelegramBotEditAPI
  :<|> TelegramBotQueriesAPI
  :<|> TelegramBotGetAPI
  :<|> TelegramBotPaymentsAPI
  :<|> TelegramBotStickersAPI

-- | Proxy for Thelegram Bot API
api :: Proxy TelegramBotAPI
api = Proxy
