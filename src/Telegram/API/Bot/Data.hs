{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}

module Telegram.API.Bot.Data
    (
      User       (..)
    , Chat       (..)
    , Message    (..)
    , PhotoSize  (..)
    , Audio      (..)
    , Document   (..)
    , Sticker    (..)
    , Video      (..)
    , Voice      (..)
    , Contact    (..)
    , Location   (..)
    ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           GHC.TypeLits

data User = User
  {
    id :: Int
  , first_name :: Text
  , last_name :: Maybe Text
  , username :: Maybe Text
  } deriving (FromJSON, ToJSON, Show, Generic)

data Contact = Contact
  {
    contact_phone_number :: Text
  , contact_first_name   :: Text
  , contact_last_name    :: Maybe Text
  , contact_user_id      :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON Contact where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 8 }

instance FromJSON Contact where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 8 }

data Chat = Chat
  {
    chat_id :: Int
  , chat_type :: Text
  , chat_title :: Maybe Text
  , chat_username :: Maybe Text
  , chat_first_name :: Maybe Text
  , chat_last_name :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON Chat where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 5 }

instance FromJSON Chat where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 5 }

data PhotoSize = PhotoSize
  {
    photo_file_id   :: Text
  , photo_width     :: Int
  , photo_height    :: Int
  , photo_file_size :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON PhotoSize where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 6 }

instance FromJSON PhotoSize where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 6 }

data Audio = Audio
  {
    audio_file_id   :: Text
  , audio_duration  :: Int
  , audio_performer :: Maybe Text
  , audio_title     :: Maybe Text
  , audio_mime_type :: Maybe Text
  , audio_file_size :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON Audio where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 6 }

instance FromJSON Audio where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 6 }

data Document = Document
  {
    doc_file_id   :: Text
  , doc_thumb     :: Maybe PhotoSize
  , doc_file_name :: Maybe Text
  , doc_mime_type :: Maybe Text
  , doc_file_size :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON Document where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 4 }

instance FromJSON Document where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 4 }

data Sticker = Sticker
  {
    sticker_file_id   :: Text
  , sticker_width     :: Int
  , sticker_height    :: Int
  , sticker_thumb     :: Maybe PhotoSize
  , sticker_file_size :: Int
  } deriving (Show, Generic)

instance ToJSON Sticker where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 8 }

instance FromJSON Sticker where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 8 }

data Video = Video
  {
    video_file_id   :: Text
  , video_width     :: Int
  , video_height    :: Int
  , video_duration  :: Int
  , video_thumb     :: Maybe PhotoSize
  , video_mime_type :: Maybe Text
  , video_file_size :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON Video where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 6 }

instance FromJSON Video where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 6 }

data Voice = Voice
  {
    voice_file_id   :: Text
  , voice_duration  :: Int
  , voice_mime_type :: Maybe Text
  , voice_file_size :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON Voice where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 6 }

instance FromJSON Voice where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 6 }

data Location = Location
  {
    longitude :: Float
  , latitude  :: Float
  } deriving (FromJSON, ToJSON, Show, Generic)

data Message = Message
  {
    message_id :: Int
  , from :: User
  , date :: Int
  , chat :: Chat
  , forward_from :: Maybe User
  , forward_date :: Maybe Int
  , reply_to_message :: Maybe Message
  , text :: Maybe Text
  , audio :: Maybe Audio
  , document :: Maybe Document
  , photo :: Maybe [PhotoSize]
  , sticker :: Maybe Sticker
  , video :: Maybe Video
  , voice :: Voice
  , caption :: Maybe Text
  , contact :: Maybe Contact
  , location :: Maybe Location
  , new_chat_participant :: Maybe User
  , left_chat_participant :: Maybe User
  , new_chat_title :: Maybe Text
  , new_chat_photo :: Maybe [PhotoSize]
  , delete_chat_photo :: Maybe Bool
  , group_chat_created :: Maybe Bool
  , supergroup_chat_created :: Maybe Bool
  , channel_chat_created :: Maybe Bool
  , migrate_to_chat_id :: Maybe Int
  , migrate_from_chat_id :: Maybe Int
  } deriving (FromJSON, ToJSON, Show, Generic)
