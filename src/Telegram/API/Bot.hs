{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}

module Telegram.API.Bot
  (
    getMe
  , sendMessage
  , sendSticker
  , TelegramBotsAPI
  , Token             (..)
  , GetMeResponse     (..)
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

newtype Token = Token Text
  deriving (Show, Eq, Ord)

instance ToText Token where
  toText (Token x) = x

instance FromText Token where
  fromText x = Just (Token (x))

type TelegramToken = Capture ":token" Token

type TelegramBotsAPI =
         TelegramToken :> "getMe"
         :> Get '[JSON] GetMeResponse
    :<|> TelegramToken :> "sendMessage"
         :> ReqBody '[JSON] SendMessageRequest
         :> Post '[JSON] SendMessageResponse
    :<|> TelegramToken :> "sendSticker"
             :> ReqBody '[JSON] SendStickerRequest
             :> Post '[JSON] SendMessageResponse

api :: Proxy TelegramBotsAPI
api = Proxy

getMe_ :: Token -> EitherT ServantError IO GetMeResponse
sendMessage_ :: Token -> SendMessageRequest -> EitherT ServantError IO SendMessageResponse
sendSticker_ :: Token -> SendStickerRequest -> EitherT ServantError IO SendMessageResponse
--getMe_ :<|> sendMessage_ = client api (BaseUrl Http "localhost" 8888)
getMe_ :<|> sendMessage_ :<|> sendSticker_ = client api (BaseUrl Https "api.telegram.org" 443)

getMe :: Token -> IO (Either ServantError GetMeResponse)
getMe token = runEitherT $ getMe_ token

sendMessage :: Token -> SendMessageRequest -> IO (Either ServantError SendMessageResponse)
sendMessage token request = runEitherT $ sendMessage_ token request

sendSticker :: Token -> SendStickerRequest -> IO (Either ServantError SendMessageResponse)
sendSticker token request = runEitherT $ sendSticker_ token request

