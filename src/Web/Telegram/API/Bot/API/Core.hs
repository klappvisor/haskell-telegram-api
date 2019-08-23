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
  , run_
  , runM
  , asking
  , runClient
  , runClient'
  , runTelegramClient
  , telegramBaseUrl
  ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader

import           Data.Text                  (Text)
import           Network.HTTP.Client        (Manager)
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

-- | Allows to run 'TelegramClient' against arbitrary url
runClient' :: TelegramClient a -> Token -> ClientEnv -> IO (Either ClientError a)
runClient' tcm token = runClientM (runReaderT tcm token)

-- | Runs 'TelegramClient'
runClient :: TelegramClient a -> Token -> Manager -> IO (Either ClientError a)
runClient tcm token manager = runClient' tcm token (ClientEnv manager telegramBaseUrl Nothing)

-- | Runs 'TelegramClient'
runTelegramClient :: Token -> Manager -> TelegramClient a -> IO (Either ClientError a)
runTelegramClient token manager tcm = runClient tcm token manager

asking :: Monad m => (t -> m b) -> ReaderT t m b
asking op = ask >>= \t -> lift $ op t

run :: BaseUrl -> (Token -> a -> ClientM b) -> Token -> a -> Manager -> IO (Either ClientError b)
run b e t r m = runClientM (e t r) (ClientEnv m b Nothing)

run_ :: Monad m => (a -> b -> m c) -> b -> ReaderT a m c
run_ act request = asking $ flip act request

runM :: (r -> TelegramClient a) -> Token -> r -> Manager -> IO (Either ClientError a)
runM tcm token request = runClient (tcm request) token
