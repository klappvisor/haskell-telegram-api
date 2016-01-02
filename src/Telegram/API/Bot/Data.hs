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

data User = User
  {
    id :: Int
  , first_name :: Text
  , last_name :: Maybe Text
  , username :: Maybe Text
  } deriving (FromJSON, ToJSON, Show, Generic)

data Chat = Chat
  {
    id :: Int
  , type' :: Text
  , title :: Maybe Text
  , username :: Maybe Text
  , first_name :: Maybe Text
  , last_name :: MAybe Text
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

data PhotoSize = PhotoSize
  {
    file_id   :: Text
  , width     :: Int
  , height    :: Int
  , file_size :: Maybe Int
  } deriving (FromJSON, ToJSON, Show, Generic)

data Audio = Audio
  {
    file_id   :: Text
  , duration  :: Int
  , performer :: Maybe Text
  , title     :: Maybe Text
  , mime_type :: Maybe Text
  , file_size :: Maybe Int
  } deriving (FromJSON, ToJSON, Show, Generic)

data Document = Document
  {
    file_id   :: Text
  , thumb     :: Maybe PhotoSize
  , file_name :: Maybe Text
  , mime_type :: Maybe Text
  , file_size :: Maybe Int
  } deriving (FromJSON, ToJSON, Show, Generic)

data Sticker = Sticker
  {
    file_id   :: Text
  , width     :: Int
  , height    :: Int
  , thumb     :: Maybe PhotoSize
  , file_size :: Int
  } deriving (FromJSON, ToJSON, Show, Generic)

data Video = Video
  {
    file_id   :: Text
  , width     :: Int
  , height    :: Int
  , duration  :: Int
  , thumb     :: Maybe PhotoSize
  , mime_type :: Maybe Text
  , file_size :: Maybe Int
  } deriving (FromJSON, ToJSON, Show, Generic)

data Voice = Voice
  {
    file_id   :: Text
  , duration  :: Int
  , mime_type :: Maybe Text
  , file_size :: Maybe Int
  } deriving (FromJSON, ToJSON, Show, Generic)

data Contact = Contact
  {
    phone_number :: Text
  , first_name   :: Text
  , last_name    :: Maybe Text
  , user_id      :: Maybe Int
  } deriving (FromJSON, ToJSON, Show, Generic)

data Location = Location
  {
    longitude :: Float
  , latitude  :: Float
  } deriving (FromJSON, ToJSON, Show, Generic)