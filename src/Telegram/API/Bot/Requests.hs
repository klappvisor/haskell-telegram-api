{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts  #-}

module Telegram.API.Bot.Requests
    ( -- * Types
      SendMessageRequest           (..)
    , SendStickerRequest           (..)
    , ForwardMessageRequest        (..)
    , SendLocationRequest          (..)
    , SendChatActionRequest        (..)
    , ParseMode                    (..)
    , ChatAction                   (..)
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

-- Send Chat Actions

data ChatAction = Typing
                | UploadPhoto
                | RecordVideo
                | UploadVideo
                | RecordAudio
                | UploadAudio
                | UploadDocument
                | FindLocation deriving (Show, Generic)

instance ToJSON ChatAction where
  toJSON Typing         = "typing"
  toJSON UploadPhoto    = "upload_photo"
  toJSON RecordVideo    = "record_video"
  toJSON UploadVideo    = "upload_video"
  toJSON RecordAudio    = "record_audio"
  toJSON UploadAudio    = "upload_audio"
  toJSON UploadDocument = "upload_cocument"
  toJSON FindLocation   = "find_location"

instance FromJSON ChatAction where
  parseJSON "typing"          = pure Typing
  parseJSON "upload_photo"    = pure UploadPhoto
  parseJSON "record_video"    = pure RecordVideo
  parseJSON "upload_video"    = pure UploadVideo
  parseJSON "record_audio"    = pure RecordAudio
  parseJSON "upload_audio"    = pure UploadAudio
  parseJSON "upload_cocument" = pure UploadDocument
  parseJSON "find_location"   = pure FindLocation

data SendChatActionRequest = SendChatActionRequest
  {
    action_chat_id :: Text
  , action_action :: ChatAction
  } deriving (Show, Generic)

instance ToJSON SendChatActionRequest where
  toJSON = toJsonDrop 7

instance FromJSON SendChatActionRequest where
  parseJSON = parseJsonDrop 7