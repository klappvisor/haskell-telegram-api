{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}

module Telegram.API.Bots
  (
    getMe
  , TelegramBotsAPI
  , Token             (..)
  , User              (..)
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

newtype Token = Token Text
  deriving (Show, Eq, Ord)

instance ToText Token where
  toText (Token x) = x

instance FromText Token where
  fromText x =Just (Token (x))

data User = User {
    id :: Int
  , first_name :: Text
  , last_name :: Maybe (Text)
  , username :: Maybe (Text)
  } deriving (FromJSON, ToJSON, Show, Generic)

data GetMeResponse = GetMeResponse {
    result :: User
  } deriving (FromJSON, ToJSON, Show, Generic)

type TelegramBotsAPI = Capture ":token" Token :> "getMe" :> Get '[JSON] GetMeResponse

api :: Proxy TelegramBotsAPI
api = Proxy

getMe' :: Token -> EitherT ServantError IO GetMeResponse
getMe' = client api (BaseUrl Https "api.telegram.org" 443)

getMe :: Token -> IO (Either ServantError GetMeResponse)
getMe token = runEitherT $ getMe' token