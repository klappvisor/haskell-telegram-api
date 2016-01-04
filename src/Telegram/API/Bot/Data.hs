{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}

module Telegram.API.Bot.Data
    ( -- * Types
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
    , Update     (..)
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

-- | This object represents a Telegram user or bot.
data User = User
  {
    user_id :: Int
  , user_first_name :: Text
  , user_last_name :: Maybe Text
  , user_username :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON User where
  toJSON = toJsonDrop 5

instance FromJSON User where
  parseJSON = parseJsonDrop 5

-- | This object represents a phone contact.
data Contact = Contact
  {
    contact_phone_number :: Text
  , contact_first_name   :: Text
  , contact_last_name    :: Maybe Text
  , contact_user_id      :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON Contact where
  toJSON = toJsonDrop 8

instance FromJSON Contact where
  parseJSON = parseJsonDrop 8

-- | This object represents a chat.
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
  toJSON = toJsonDrop 5

instance FromJSON Chat where
  parseJSON = parseJsonDrop 5

-- | This object represents one size of a photo or a `File` / `Sticker` thumbnail.
data PhotoSize = PhotoSize
  {
    photo_file_id   :: Text
  , photo_width     :: Int
  , photo_height    :: Int
  , photo_file_size :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON PhotoSize where
  toJSON = toJsonDrop 6

instance FromJSON PhotoSize where
  parseJSON = parseJsonDrop 6

-- | This object represents an audio file to be treated as music by the Telegram clients.
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
  toJSON = toJsonDrop 6

instance FromJSON Audio where
  parseJSON = parseJsonDrop 6

-- | This object represents a general file (as opposed to `PhotoSize`, `Voice` messages and `Audio` files).
data Document = Document
  {
    doc_file_id   :: Text
  , doc_thumb     :: Maybe PhotoSize
  , doc_file_name :: Maybe Text
  , doc_mime_type :: Maybe Text
  , doc_file_size :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON Document where
  toJSON = toJsonDrop 4

instance FromJSON Document where
  parseJSON = parseJsonDrop 4

-- | This object represents a sticker.
data Sticker = Sticker
  {
    sticker_file_id   :: Text
  , sticker_width     :: Int
  , sticker_height    :: Int
  , sticker_thumb     :: Maybe PhotoSize
  , sticker_file_size :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON Sticker where
  toJSON = toJsonDrop 8

instance FromJSON Sticker where
  parseJSON = parseJsonDrop 8

-- | This object represents a video file.
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
  toJSON = toJsonDrop 6

instance FromJSON Video where
  parseJSON = parseJsonDrop 6

-- | This object represents a voice note.
data Voice = Voice
  {
    voice_file_id   :: Text
  , voice_duration  :: Int
  , voice_mime_type :: Maybe Text
  , voice_file_size :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON Voice where
  toJSON = toJsonDrop 6

instance FromJSON Voice where
  parseJSON = parseJsonDrop 6

-- | This object represents an incoming update.
-- Only one of the optional parameters can be present in any given update.
data Update = Update
  {
    update_id :: Int
  , message :: Message
  } deriving (FromJSON, ToJSON, Show, Generic)

-- | This object represents a point on the map.
data Location = Location
  {
    longitude :: Float
  , latitude  :: Float
  } deriving (FromJSON, ToJSON, Show, Generic)

-- | This object represents a message.
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
  , voice :: Maybe Voice
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
