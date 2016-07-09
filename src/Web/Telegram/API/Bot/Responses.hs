{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains responses from Telegram Bot API
module Web.Telegram.API.Bot.Responses
    ( -- * Types
      Response                        (..)
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
    ) where

import           Data.Aeson
import           GHC.Generics
import           Web.Telegram.API.Bot.Data
import           Web.Telegram.API.Bot.JsonExt

data Response a = Response
  {
    result :: a
  } deriving (Show, Generic, FromJSON)

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