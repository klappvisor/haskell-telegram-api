{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.API.Bots where

import Data.Aeson
import Data.Aeson.TH
import Servant.API
import Servant.Client
import Data.Text (Text)
import GHC.Generics (Generic)

newtype Token = Token Text
  deriving (ToText, FromText, Show, Eq, Ord)

data User = User {
    id :: Int
  , first_name :: Text
  , last_name :: Maybe (Text)
  , username :: Maybe (Text)
  } deriving (Show, Generic)

data GetMeResponse = GetMeResponse {
    result :: User
  } deriving (Show, Eq, Ord, Generic)

instance FromJSON GetMeResponse

type TelegramBotsAPI = "users" :> Get '[JSON] [User]

getMe' :: Token -> EitherT ServantError IO GetMeResponse
getMe' token = client api (BaseUrl Https ("api.telegram.org/bot" <> token) 443)

getMe :: Token -> IO (Either ServantError GetMeResponse)
getMe token = runEitherT $ getMe' token


