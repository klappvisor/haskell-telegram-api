{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Web.Telegram.API.Bot.API.Core
  (  -- * Types
    Token             (..)
  , TelegramToken
  , run
  , telegramBaseUrl
  ) where

import           Data.Text (Text)
import           Network.HTTP.Client (Manager)
import           Servant.API
import           Servant.Client

-- | Telegram Bot's Token
newtype Token = Token Text
  deriving (Show, Eq, Ord, ToHttpApiData, FromHttpApiData)

-- | Type for token
type TelegramToken = Capture ":token" Token

telegramBaseUrl :: BaseUrl
telegramBaseUrl = BaseUrl Https "api.telegram.org" 443 ""

run :: BaseUrl -> (Token -> a -> ClientM b) -> Token -> a -> Manager -> IO (Either ServantError b)
run b e t r m = runClientM (e t r) (ClientEnv m b)
