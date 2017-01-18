{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Web.Telegram.API.Bot.API
  ( -- * Functions
    module API
  , runClient
  , runClient'
    -- * API
  , TelegramBotAPI
  , api
    -- * Types
  , Token             (..)
  ) where

import           Data.Proxy
import           Servant.API
import           Web.Telegram.API.Bot.API.Chats    as API
import           Web.Telegram.API.Bot.API.Core
import           Web.Telegram.API.Bot.API.Messages as API
import           Web.Telegram.API.Bot.API.Updates  as API

type TelegramBotAPI =
       TelegramBotMessagesAPI
  :<|> TelegramBotUpdatesAPI
  :<|> TelegramBotChatsAPI

-- | Proxy for Thelegram Bot API
api :: Proxy TelegramBotAPI
api = Proxy
