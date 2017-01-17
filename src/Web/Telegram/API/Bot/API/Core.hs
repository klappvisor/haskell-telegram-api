{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Web.Telegram.API.Bot.API.Core
  (  -- * Types
    Token             (..)
  , TelegramToken
  , TelegramClient
  , run
  , asking
  , runClient
  , runClient'
  , telegramBaseUrl
  ) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

import           Data.Text (Text)
import           Network.HTTP.Client (Manager)
import           Servant.API
import           Servant.Client

-- | Telegram Bot's Token
newtype Token = Token Text
  deriving (Show, Eq, Ord, ToHttpApiData, FromHttpApiData)

-- | Type for token
type TelegramToken = Capture ":token" Token

type TelegramClient a = ReaderT Token ClientM a

telegramBaseUrl :: BaseUrl
telegramBaseUrl = BaseUrl Https "api.telegram.org" 443 ""

runClient' :: TelegramClient a -> Token -> ClientEnv -> IO (Either ServantError a)
runClient' tcm token = runClientM (runReaderT tcm token)

runClient :: TelegramClient a -> Token -> Manager -> IO (Either ServantError a)
runClient tcm token manager = runClient' tcm token (ClientEnv manager telegramBaseUrl)

asking :: Monad m => (t -> m b) -> ReaderT t m b
asking op = ask >>= \t -> lift $ op t

run :: BaseUrl -> (Token -> a -> ClientM b) -> Token -> a -> Manager -> IO (Either ServantError b)
run b e t r m = runClientM (e t r) (ClientEnv m b)
