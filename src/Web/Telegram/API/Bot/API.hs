{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Web.Telegram.API.Bot.API
  ( -- * Functions
    module API
    -- * API
  , TelegramBotAPI
  , api
    -- * Types
  , Token             (..)
  ) where

import           Data.Proxy
import           Servant.API
import           Web.Telegram.API.Bot.API.Core
import           Web.Telegram.API.Bot.API.Messages as API
import           Web.Telegram.API.Bot.API.Settings as API
import           Web.Telegram.API.Bot.API.Chats as API

type TelegramBotAPI =
       TelegramBotMessagesAPI
  :<|> TelegramBotSettingsAPI
  :<|> TelegramBotChatsAPI

-- | Proxy for Thelegram Bot API
api :: Proxy TelegramBotAPI
api = Proxy
