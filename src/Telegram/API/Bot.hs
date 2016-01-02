{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}

module Telegram.API.Bot
  (
    getMe,
    sendMessage
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

newtype Token = Token Text
  deriving (Show, Eq, Ord)

instance ToText Token where
  toText (Token x) = x

instance FromText Token where
  fromText x = Just (Token (x))

type TelegramBotsAPI = Capture ":token" Token :> "getMe" :> Get '[JSON] GetMeResponse
                  :<|> Capture ":token" Token :> "sendMessage" :> QueryParam "chat_id" Int :> QueryParam "text" Text :> Get '[JSON] SendMessageResponse

api :: Proxy TelegramBotsAPI
api = Proxy

getMe' :: Token -> EitherT ServantError IO GetMeResponse
sendMessage' :: Token -> Maybe Int -> Maybe Text -> EitherT ServantError IO SendMessageResponse
--getMe' :<|> sendMessage' = client api (BaseUrl Http "localhost" 8888)
getMe' :<|> sendMessage' = client api (BaseUrl Https "api.telegram.org" 443)

getMe :: Token -> IO (Either ServantError GetMeResponse)
getMe token = runEitherT $ getMe' token

sendMessage :: Token -> Int -> Text -> IO (Either ServantError SendMessageResponse)
sendMessage token chatId text = runEitherT $ sendMessage' token (Just chatId) (Just text)