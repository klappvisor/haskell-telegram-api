{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}

module Telegram.API.Bot.API
  ( -- * Functions
    getMe
  , sendMessage
  , sendSticker
  , forwardMessage
  , sendLocation
  , sendChatAction
  , getUpdates
    -- * API
  , TelegramBotAPI
  , api
    -- * Types
  , Token             (..)
  ) where

import           Control.Applicative
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           GHC.TypeLits
import           Servant.API
import           Servant.Client
import           Telegram.API.Bot.Data
import           Telegram.API.Bot.Responses
import           Telegram.API.Bot.Requests

-- | Telegram Bot's Token
newtype Token = Token Text
  deriving (Show, Eq, Ord)

instance ToText Token where
  toText (Token x) = x

instance FromText Token where
  fromText x = Just (Token (x))

-- | Type for token
type TelegramToken = Capture ":token" Token

-- | Telegram Bot API
type TelegramBotAPI =
         TelegramToken :> "getMe"
         :> Get '[JSON] GetMeResponse
    :<|> TelegramToken :> "sendMessage"
         :> ReqBody '[JSON] SendMessageRequest
         :> Post '[JSON] MessageResponse
    :<|> TelegramToken :> "sendSticker"
         :> ReqBody '[JSON] SendStickerRequest
         :> Post '[JSON] MessageResponse
    :<|> TelegramToken :> "forwardMessage"
         :> ReqBody '[JSON] ForwardMessageRequest
         :> Post '[JSON] MessageResponse
    :<|> TelegramToken :> "sendLocation"
         :> ReqBody '[JSON] SendLocationRequest
         :> Post '[JSON] MessageResponse
    :<|> TelegramToken :> "sendChatAction"
         :> ReqBody '[JSON] SendChatActionRequest
         :> Post '[JSON] ChatActionResponse
    :<|> TelegramToken :> "getUpdates"
         :> QueryParam "offset" Int
         :> QueryParam "limit" Int
         :> QueryParam "timeout" Int
         :> Get '[JSON] UpdatesResponse

-- | Proxy for Thelegram Bot API
api :: Proxy TelegramBotAPI
api = Proxy

getMe_          :: Token -> EitherT ServantError IO GetMeResponse
sendMessage_    :: Token -> SendMessageRequest -> EitherT ServantError IO MessageResponse
sendSticker_    :: Token -> SendStickerRequest -> EitherT ServantError IO MessageResponse
forwardMessage_ :: Token -> ForwardMessageRequest -> EitherT ServantError IO MessageResponse
sendLocation_   :: Token -> SendLocationRequest -> EitherT ServantError IO MessageResponse
sendChatAction_ :: Token -> SendChatActionRequest -> EitherT ServantError IO ChatActionResponse
getUpdates_     :: Token -> Maybe Int -> Maybe Int -> Maybe Int -> EitherT ServantError IO UpdatesResponse
getMe_
  :<|> sendMessage_
  :<|> sendSticker_
  :<|> forwardMessage_
  :<|> sendLocation_
  :<|> sendChatAction_
  :<|> getUpdates_ =
      client api
  --      (BaseUrl Http "localhost" 8888)
          (BaseUrl Https "api.telegram.org" 443)
-- | A simple method for testing your bot's auth token. Requires no parameters.
--   Returns basic information about the bot in form of a 'User' object.
getMe :: Token -> IO (Either ServantError GetMeResponse)
getMe token = runEitherT $ getMe_ token

-- | Use this method to send text messages. On success, the sent 'Message' is returned.
sendMessage :: Token -> SendMessageRequest -> IO (Either ServantError MessageResponse)
sendMessage token request = runEitherT $ sendMessage_ token request

-- | Use this method to send .webp stickers. On success, the sent 'Message' is returned.
sendSticker :: Token -> SendStickerRequest -> IO (Either ServantError MessageResponse)
sendSticker token request = runEitherT $ sendSticker_ token request

-- | Use this method to forward messages of any kind. On success, the sent 'Message' is returned.
forwardMessage :: Token -> ForwardMessageRequest -> IO (Either ServantError MessageResponse)
forwardMessage token request = runEitherT $ forwardMessage_ token request

-- | Use this method to send point on the map. On success, the sent 'Message' is returned.
sendLocation :: Token -> SendLocationRequest -> IO (Either ServantError MessageResponse)
sendLocation token request = runEitherT $ sendLocation_ token request

-- | Use this method when you need to tell the user that something is happening on the bot's side.
--   The status is set for 5 seconds or less (when a message arrives from your bot,
--   Telegram clients clear its typing status).
sendChatAction :: Token -> SendChatActionRequest -> IO (Either ServantError ChatActionResponse)
sendChatAction token request = runEitherT $ sendChatAction_ token request

-- | Use this method to receive incoming updates using long polling. An Array of 'Update' objects is returned.
getUpdates :: Token -> Maybe Int -> Maybe Int -> Maybe Int -> IO (Either ServantError UpdatesResponse)
getUpdates token offset limit timeout = runEitherT $ getUpdates_ token offset limit timeout