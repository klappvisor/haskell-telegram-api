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
    , ForwardMessageRequest        (..)
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

-- Send Message
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

-- Forward Message
data ForwardMessageRequest = ForwardMessageRequest
  {
    forward_chat_id :: Text
  , forward_from_chat_id :: Text
  , forward_mesage_id :: Int
  } deriving (Show, Generic)

instance ToJSON ForwardMessageRequest where
  toJSON = toJsonDrop 8

instance FromJSON ForwardMessageRequest where
  parseJSON = parseJsonDrop 8

-- Send Sticker

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

-- Send Location

data SendLocationRequest = SendLocationRequest
  {
    location_chat_id :: Text
  , location_latitude :: Float
  , location_longitude :: Float
  , location_reply_to_message_id :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON SendLocationRequest where
  toJSON = toJsonDrop 9

instance FromJSON SendLocationRequest where
  parseJSON = parseJsonDrop 9