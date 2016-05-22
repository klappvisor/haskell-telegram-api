{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains responses from Telegram Bot API
module Web.Telegram.API.Bot.Responses
    ( -- * Types
      GetMeResponse                   (..)
    , MessageResponse                 (..)
    , ChatActionResponse              (..)
    , UpdatesResponse                 (..)
    , FileResponse                    (..)
    , UserProfilePhotosResponse       (..)
    , SetWebhookResponse              (..)
    , InlineQueryResponse             (..)
    , CallbackQueryResponse           (..)
    , KickChatMemberResponse          (..)
    , LeaveChatResponse               (..)
    , UnbanChatMemberResponse         (..)
    , GetChatResponse                 (..)
    , GetChatAdministratorsResponse   (..)
    , GetChatMembersCountResponse     (..)
    , GetChatMemberResponse           (..)
    ) where

import           Data.Aeson
import           GHC.Generics
import           Web.Telegram.API.Bot.Data
import           Web.Telegram.API.Bot.JsonExt

-- TODO: use generic response i.e. `data Response a = Response `

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

-- | This object represents 'answerCallbackQuery' response
data CallbackQueryResponse = CallbackQueryResponse
  {
    callback_result :: Bool
  } deriving (Show, Generic)

instance ToJSON CallbackQueryResponse where
  toJSON = toJsonDrop 9

instance FromJSON CallbackQueryResponse where
  parseJSON = parseJsonDrop 9

-- | This object represents 'kickChatMember' response
data KickChatMemberResponse = KickChatMemberResponse
  {
    kick_result :: Bool
  } deriving (Show, Generic)

instance ToJSON KickChatMemberResponse where
  toJSON = toJsonDrop 5

instance FromJSON KickChatMemberResponse where
  parseJSON = parseJsonDrop 5

data LeaveChatResponse = LeaveChatResponse
  {
    leave_result :: Bool
  } deriving (Show, Generic)

instance ToJSON LeaveChatResponse where
  toJSON = toJsonDrop 6

instance FromJSON LeaveChatResponse where
  parseJSON = parseJsonDrop 6

-- | This object represents 'unbanChatMember' response
data UnbanChatMemberResponse = UnbanChatMemberResponse
  {
    unban_result :: Bool
  } deriving (Show, Generic)

instance ToJSON UnbanChatMemberResponse where
  toJSON = toJsonDrop 6

instance FromJSON UnbanChatMemberResponse where
  parseJSON = parseJsonDrop 6

data GetChatResponse = GetChatResponse
  {
    chat_result :: Chat
  } deriving (Show, Generic)

instance ToJSON GetChatResponse where
  toJSON = toJsonDrop 5

instance FromJSON GetChatResponse where
  parseJSON = parseJsonDrop 5

data GetChatAdministratorsResponse = GetChatAdministratorsResponse
  {
    ca_result :: [ChatMember]
  } deriving (Show, Generic)

instance ToJSON GetChatAdministratorsResponse where
  toJSON = toJsonDrop 3

instance FromJSON GetChatAdministratorsResponse where
  parseJSON = parseJsonDrop 3

data GetChatMembersCountResponse = GetChatMembersCountResponse
  {
    cmc_result :: Int
  } deriving (Show, Generic)

instance ToJSON GetChatMembersCountResponse where
  toJSON = toJsonDrop 4

instance FromJSON GetChatMembersCountResponse where
  parseJSON = parseJsonDrop 4

data GetChatMemberResponse = GetChatMemberResponse
  {
    gcm_result :: Int
  } deriving (Show, Generic)

instance ToJSON GetChatMemberResponse where
  toJSON = toJsonDrop 4

instance FromJSON GetChatMemberResponse where
  parseJSON = parseJsonDrop 4