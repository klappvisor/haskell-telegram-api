{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Web.Telegram.API.Bot.API.Settings
  ( -- * Functions
    getMe
  , getUpdates
  , setWebhook
  , setWebhookWithCertificate
  , deleteWebhook
  , getWebhookInfo
    -- * API
  , TelegramBotSettingsAPI
  , settingsApi
  ) where

import           Data.Proxy
import           Data.Text (Text)
import           Network.HTTP.Client (Manager)
import           Servant.API
import           Servant.Client
import           Servant.Client.MultipartFormData
import           Web.Telegram.API.Bot.Responses
import           Web.Telegram.API.Bot.Requests
import           Web.Telegram.API.Bot.API.Core

-- | Telegram Bot API
type TelegramBotSettingsAPI =
         TelegramToken :> "getMe"
         :> Get '[JSON] GetMeResponse
    :<|> TelegramToken :> "getUpdates"
         :> QueryParam "offset" Int
         :> QueryParam "limit" Int
         :> QueryParam "timeout" Int
         :> Get '[JSON] UpdatesResponse
    :<|> TelegramToken :> "setWebhook"
         :> QueryParam "url" Text
         :> Get '[JSON] SetWebhookResponse
    :<|> TelegramToken :> "setWebhook"
         :> MultipartFormDataReqBody SetWebhookRequest
         :> Post '[JSON] SetWebhookResponse
    :<|> TelegramToken :> "deleteWebhook"
         :> Post '[JSON] (Response Bool)
    :<|> TelegramToken :> "getWebhookInfo"
         :> Get '[JSON] GetWebhookInfoResponse

-- | Proxy for Thelegram Bot API to configure your bot
settingsApi :: Proxy TelegramBotSettingsAPI
settingsApi = Proxy

getMe_                     :: Token -> ClientM GetMeResponse
getUpdates_                :: Token -> Maybe Int -> Maybe Int -> Maybe Int -> ClientM UpdatesResponse
setWebhook_                :: Token -> Maybe Text -> ClientM SetWebhookResponse
setWebhookWithCert_        :: Token -> SetWebhookRequest -> ClientM SetWebhookResponse
deleteWebhook_             :: Token -> ClientM (Response Bool)
getWebhookInfo_            :: Token -> ClientM GetWebhookInfoResponse
getMe_
  :<|> getUpdates_
  :<|> setWebhook_
  :<|> setWebhookWithCert_
  :<|> deleteWebhook_
  :<|> getWebhookInfo_ = client settingsApi

-- | A simple method for testing your bot's auth token. Requires no parameters.
--   Returns basic information about the bot in form of a 'User' object.
getMe :: Token -> Manager -> IO (Either ServantError GetMeResponse)
getMe token manager = runClientM (getMe_ token) (ClientEnv manager telegramBaseUrl)

-- | Use this method to receive incoming updates using long polling. An Array of 'Update' objects is returned.
getUpdates
    :: Token
    -> Maybe Int -- ^ offset
    -> Maybe Int -- ^ limit
    -> Maybe Int -- ^ timeout
    -> Manager
    -> IO (Either ServantError UpdatesResponse)
getUpdates token offset limit timeout manager = runClientM (getUpdates_ token offset limit timeout) $ ClientEnv manager telegramBaseUrl

-- | Use this method to specify a url and receive incoming updates via an outgoing webhook. Whenever there is an update for the bot, we will send an HTTPS POST request to the specified url, containing a JSON-serialized 'Update'. In case of an unsuccessful request, we will give up after a reasonable amount of attempts.
--
--       If you'd like to make sure that the Webhook request comes from Telegram, we recommend using a secret path in the URL, e.g. @https://www.example.com/<token>@. Since nobody else knows your bot‘s token, you can be pretty sure it’s us.
setWebhook :: Token
    -> Maybe Text -- ^ HTTPS url to send updates to. Use an empty string to remove webhook integration
    -> Manager
    -> IO (Either ServantError SetWebhookResponse)
setWebhook token url manager = runClientM (setWebhook_ token url) $ ClientEnv manager telegramBaseUrl

-- | Use this method to specify a url and receive incoming updates via an outgoing webhook. Whenever there is an update for the bot, we will send an HTTPS POST request to the specified url, containing a JSON-serialized 'Update'. In case of an unsuccessful request, we will give up after a reasonable amount of attempts.
--
--       If you'd like to make sure that the Webhook request comes from Telegram, we recommend using a secret path in the URL, e.g. @https://www.example.com/<token>@. Since nobody else knows your bot‘s token, you can be pretty sure it’s us.
setWebhookWithCertificate :: Token -> SetWebhookRequest -> Manager -> IO (Either ServantError SetWebhookResponse)
setWebhookWithCertificate = run telegramBaseUrl setWebhookWithCert_

deleteWebhook :: Token -> Manager -> IO (Either ServantError (Response Bool))
deleteWebhook token manager = runClientM (deleteWebhook_ token) $ ClientEnv manager telegramBaseUrl

-- | Contains information about the current status of a webhook.
getWebhookInfo :: Token -> Manager -> IO (Either ServantError GetWebhookInfoResponse)
getWebhookInfo token manager = runClientM (getWebhookInfo_ token) $ ClientEnv manager telegramBaseUrl
