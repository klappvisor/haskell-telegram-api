{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts  #-}

-- | This module contains responses from Telegram Bot API
module Web.Telegram.API.Bot.Responses
    ( -- * Types
      GetMeResponse             (..)
    , MessageResponse           (..)
    , ChatActionResponse        (..)
    , UpdatesResponse           (..)
    , FileResponse              (..)
    , UserProfilePhotosResponse (..)
    , SetWebhookResponse        (..)
    , InlineQueryResponse       (..)
    ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           GHC.TypeLits
import           Web.Telegram.API.Bot.Data
import           Web.Telegram.API.Bot.JsonExt

-- | This object represents 'getMe' response
data GetMeResponse = GetMeResponse
  {
    user_result :: User
  } deriving (Show, Generic)

instance ToJSON GetMeResponse where
  toJSON = toJsonDrop 5

instance FromJSON GetMeResponse where
  parseJSON = parseJsonDrop 5

-- | This object represents message response
data MessageResponse = MessageResponse
  {
    message_result :: Message
  } deriving (Show, Generic)

instance ToJSON MessageResponse where
  toJSON = toJsonDrop 8

instance FromJSON MessageResponse where
  parseJSON = parseJsonDrop 8

-- | This object represents 'sendChatAction' response
data ChatActionResponse = ChatActionResponse
  {
    action_result :: Bool
  } deriving (Show, Generic)

instance ToJSON ChatActionResponse where
  toJSON = toJsonDrop 7

instance FromJSON ChatActionResponse where
  parseJSON = parseJsonDrop 7

-- | This object represents 'getUpdates' response
data UpdatesResponse = UpdatesResponse
  {
    update_result :: [Update]
  } deriving (Show, Generic)

instance ToJSON UpdatesResponse where
  toJSON = toJsonDrop 7

instance FromJSON UpdatesResponse where
  parseJSON = parseJsonDrop 7

-- | This object represents file response
data FileResponse = FileResponse
  {
    file_result :: File
  } deriving (Show, Generic)

instance ToJSON FileResponse where
  toJSON = toJsonDrop 5

instance FromJSON FileResponse where
  parseJSON = parseJsonDrop 5

-- | This object represents user profile photos response
data UserProfilePhotosResponse = UserProfilePhotosResponse
  {
    photos_result :: UserProfilePhotos
  } deriving (Show, Generic)

instance ToJSON UserProfilePhotosResponse where
  toJSON = toJsonDrop 7

instance FromJSON UserProfilePhotosResponse where
  parseJSON = parseJsonDrop 7

-- | This object represents 'setWebhook' response
data SetWebhookResponse = SetWebhookResponse
  {
    webhook_result :: Bool
  } deriving (Show, Generic)

instance ToJSON SetWebhookResponse where
  toJSON = toJsonDrop 8

instance FromJSON SetWebhookResponse where
  parseJSON = parseJsonDrop 8

-- | This object represents 'answerInlineQuery' response
data InlineQueryResponse = InlineQueryResponse
  {
    query_result :: Bool
  } deriving (Show, Generic)

instance ToJSON InlineQueryResponse where
  toJSON = toJsonDrop 6

instance FromJSON InlineQueryResponse where
  parseJSON = parseJsonDrop 6