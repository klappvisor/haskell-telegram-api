{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts  #-}

module Telegram.API.Bot.Requests
    (
      SendMessageRequest           (..)
    , SendStickerRequest           (..)
    , ParseMode                    (..)
    ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           GHC.TypeLits
import           Telegram.API.Bot.JsonExt

data ParseMode = Markdown deriving (Show, Generic)

instance ToJSON ParseMode where
  toJSON Markdown = "Markdown"

instance FromJSON ParseMode where
  parseJSON "Markdown" = pure $ Markdown
  parseJSON _          = fail "Failed to parse ParseMode"

data SendMessageRequest = SendMessageRequest
  {
    message_chat_id                  :: Text
  , message_text                     :: Text
  , message_parse_mode               :: Maybe ParseMode
  , message_disable_web_page_preview :: Maybe Bool
  , message_reply_to_message_id      :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON SendMessageRequest where
  toJSON = toJsonDrop 8

instance FromJSON SendMessageRequest where
  parseJSON = parseJsonDrop 8

data SendStickerRequest = SendStickerRequest
  {
    sticker_chat_id                  :: Text
  , sticker_sticker                  :: Text
  , sticker_reply_to_message_id      :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON SendStickerRequest where
  toJSON = toJsonDrop 8

instance FromJSON SendStickerRequest where
  parseJSON = parseJsonDrop 8