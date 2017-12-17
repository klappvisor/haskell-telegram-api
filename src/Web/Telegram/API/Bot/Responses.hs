{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains responses from Telegram Bot API
module Web.Telegram.API.Bot.Responses
    ( -- * Types
      Response                        (..)
    , ResponseParameters              (..)
    , GetMeResponse
    , MessageResponse
    , ChatActionResponse
    , UpdatesResponse
    , FileResponse
    , UserProfilePhotosResponse
    , SetWebhookResponse
    , InlineQueryResponse
    , CallbackQueryResponse
    , KickChatMemberResponse
    , LeaveChatResponse
    , UnbanChatMemberResponse
    , GetChatResponse
    , GetChatAdministratorsResponse
    , GetChatMembersCountResponse
    , GetChatMemberResponse
    , GetWebhookInfoResponse
    , AnswerShippingQueryResponse
    , AnswerPreCheckoutQueryResponse
    ) where

import           Data.Aeson
import           Data.Int                     (Int64)
import           GHC.Generics
import           Web.Telegram.API.Bot.Data
import           Web.Telegram.API.Bot.JsonExt

data Response a = Response
  {
    result     :: a
  , parameters :: Maybe ResponseParameters
  } deriving (Show, Generic, FromJSON)

data ResponseParameters = ResponseParameters
  {
    res_migrate_to_chat_id :: Maybe Int64 -- ^ The group has been migrated to a supergroup with the specified identifier. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.
  , res_retry_after        :: Maybe Int   -- ^ In case of exceeding flood control, the number of seconds left to wait before the request can be repeated
  } deriving (Show, Generic)

instance FromJSON ResponseParameters where
  parseJSON = parseJsonDrop 4

-- | This object represents 'getMe' response
type GetMeResponse = Response User

-- | This object represents message response
type MessageResponse = Response Message

-- | This object represents 'sendChatAction' response
type ChatActionResponse = Response Bool

-- | This object represents 'getUpdates' response
type UpdatesResponse = Response [Update]

-- | This object represents file response
type FileResponse = Response File

-- | This object represents user profile photos response
type UserProfilePhotosResponse = Response UserProfilePhotos

-- | This object represents 'setWebhook' response
type SetWebhookResponse = Response Bool

-- | This object represents 'answerInlineQuery' response
type InlineQueryResponse = Response Bool

-- | This object represents 'answerCallbackQuery' response
type CallbackQueryResponse = Response Bool

-- | This object represents 'kickChatMember' response
type KickChatMemberResponse = Response Bool

type LeaveChatResponse = Response Bool

-- | This object represents 'unbanChatMember' response
type UnbanChatMemberResponse = Response Bool

type GetChatResponse = Response Chat

type GetChatAdministratorsResponse = Response [ChatMember]

type GetChatMembersCountResponse = Response Int

type GetChatMemberResponse = Response ChatMember

type GetWebhookInfoResponse = Response WebhookInfo

type AnswerShippingQueryResponse = Response Bool

type AnswerPreCheckoutQueryResponse = Response Bool
