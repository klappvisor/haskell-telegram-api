{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts  #-}

-- | This module contains data objects which represents requests to Telegram Bot API
module Web.Telegram.API.Bot.Requests
    ( -- * Types
      SendMessageRequest           (..)
    , ForwardMessageRequest        (..)
    , SendPhotoRequest             (..)
    , SendAudioRequest             (..)
    , SendDocumentRequest          (..)
    , SendStickerRequest           (..)
    , SendVideoRequest             (..)
    , SendVoiceRequest             (..)
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
import           Web.Telegram.API.Bot.JsonExt

-- | Parse mode for text message
data ParseMode = Markdown deriving (Show, Generic)

instance ToJSON ParseMode where
  toJSON Markdown = "Markdown"

instance FromJSON ParseMode where
  parseJSON "Markdown" = pure $ Markdown
  parseJSON _          = fail "Failed to parse ParseMode"

-- | This object represents request for 'sendMessage'
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

-- | This object represents request for 'forwardMessage'
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

-- | This object represents request for 'sendPhoto'
data SendPhotoRequest = SendPhotoRequest
  {
    photo_chat_id             :: Text
  , photo_photo               :: Text
  , photo_caption             :: Maybe Text
  , photo_reply_to_message_id :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON SendPhotoRequest where
  toJSON = toJsonDrop 6

instance FromJSON SendPhotoRequest where
  parseJSON = parseJsonDrop 6

-- | This object represents request for 'sendAudio'
data SendAudioRequest = SendAudioRequest
  {
    _audio_chat_id             :: Text
  , _audio_audio               :: Text
  , _audio_duration            :: Maybe Int
  , _audio_performer           :: Maybe Text
  , _audio_title               :: Maybe Text
  , _audio_reply_to_message_id :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON SendAudioRequest where
  toJSON = toJsonDrop 7

instance FromJSON SendAudioRequest where
  parseJSON = parseJsonDrop 7

-- | This object represents request for 'sendSticker'
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

-- | This object represents request for 'sendDocument'
data SendDocumentRequest = SendDocumentRequest
  {
    document_chat_id                  :: Text
  , document_document                 :: Text
  , document_reply_to_message_id      :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON SendDocumentRequest where
  toJSON = toJsonDrop 9

instance FromJSON SendDocumentRequest where
  parseJSON = parseJsonDrop 9

-- | This object represents request for 'sendVideo'
data SendVideoRequest = SendVideoRequest
  {
    _video_chat_id                  :: Text
  , _video_video                    :: Text
  , _video_duration                 :: Maybe Int
  , _video_caption                  :: Maybe Text
  , _video_reply_to_message_id      :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON SendVideoRequest where
  toJSON = toJsonDrop 7

instance FromJSON SendVideoRequest where
  parseJSON = parseJsonDrop 7

-- | This object represents request for 'sendVoice'
data SendVoiceRequest = SendVoiceRequest
  {
    _voice_chat_id                  :: Text
  , _voice_voice                    :: Text
  , _voice_duration                 :: Maybe Int
  , _voice_reply_to_message_id      :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON SendVoiceRequest where
  toJSON = toJsonDrop 7

instance FromJSON SendVoiceRequest where
  parseJSON = parseJsonDrop 7

-- | This object represents request for 'sendLocation'
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

-- | Type of action to broadcast.
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

-- | This object represents request for 'sendChatAction'
data SendChatActionRequest = SendChatActionRequest
  {
    action_chat_id :: Text
  , action_action :: ChatAction
  } deriving (Show, Generic)

instance ToJSON SendChatActionRequest where
  toJSON = toJsonDrop 7

instance FromJSON SendChatActionRequest where
  parseJSON = parseJsonDrop 7