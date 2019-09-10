{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Web.Telegram.API.Bot.API.Updates
  ( -- * Functions
    getUpdates
  , getUpdatesM
  , getUpdatesM'
  , setWebhook
  , setWebhookM
  , setWebhookWithCertificate
  , setWebhookWithCertificateM
  , deleteWebhook
  , deleteWebhookM
  , getWebhookInfo
  , getWebhookInfoM
    -- * API
  , TelegramBotUpdatesAPI
  , updatesApi
  ) where

import           Data.Maybe
import           Data.Proxy
import           Data.Text                        (Text, empty)
import           Network.HTTP.Client              (Manager)
import           Servant.API
import           Servant.Client            hiding (Response)
import           Servant.Client.MultipartFormData
import           Web.Telegram.API.Bot.API.Core
import           Web.Telegram.API.Bot.Requests
import           Web.Telegram.API.Bot.Responses

-- | Telegram Bot API
type TelegramBotUpdatesAPI =
         TelegramToken :> "getUpdates"
         :> ReqBody '[JSON] GetUpdatesRequest
         :> Post '[JSON] UpdatesResponse
    :<|> TelegramToken :> "setWebhook"
         :> ReqBody '[JSON] SetWebhookRequest
         :> Post '[JSON] SetWebhookResponse
    :<|> TelegramToken :> "setWebhook"
         :> MultipartFormDataReqBody SetWebhookRequest
         :> Post '[JSON] SetWebhookResponse
    :<|> TelegramToken :> "deleteWebhook"
         :> Post '[JSON] (Response Bool)
    :<|> TelegramToken :> "getWebhookInfo"
         :> Get '[JSON] GetWebhookInfoResponse

-- | Proxy for Thelegram Bot API to configure your bot
updatesApi :: Proxy TelegramBotUpdatesAPI
updatesApi = Proxy

getUpdates_                :: Token -> GetUpdatesRequest -> ClientM UpdatesResponse
setWebhook_                :: Token -> SetWebhookRequest -> ClientM SetWebhookResponse
setWebhookWithCert_        :: Token -> SetWebhookRequest -> ClientM SetWebhookResponse
deleteWebhook_             :: Token -> ClientM (Response Bool)
getWebhookInfo_            :: Token -> ClientM GetWebhookInfoResponse
getUpdates_
  :<|> setWebhook_
  :<|> setWebhookWithCert_
  :<|> deleteWebhook_
  :<|> getWebhookInfo_ = client updatesApi

-- | Use this method to receive incoming updates using long polling. A list of 'Update' objects is returned. Use `getUpdatesM` for more features
getUpdates
    :: Token
    -> Maybe Int -- ^ offset
    -> Maybe Int -- ^ limit
    -> Maybe Int -- ^ timeout
    -> Manager
    -> IO (Either ClientError UpdatesResponse)
getUpdates token offset limit timeout = runClient (getUpdatesM request) token
    where request = GetUpdatesRequest offset limit timeout Nothing

-- | Get update with default parameters See 'getUpdates' for details.
getUpdatesM' :: TelegramClient UpdatesResponse
getUpdatesM' = getUpdatesM getUpdatesRequest

-- | See 'getUpdates'
getUpdatesM :: GetUpdatesRequest -> TelegramClient UpdatesResponse
getUpdatesM = run_ getUpdates_

-- | Use this method to specify a url and receive incoming updates via an outgoing webhook. Whenever there is an update for the bot, we will send an HTTPS POST request to the specified url, containing a JSON-serialized 'Update'. In case of an unsuccessful request, we will give up after a reasonable amount of attempts.
--
--       If you'd like to make sure that the Webhook request comes from Telegram, we recommend using a secret path in the URL, e.g. @https://www.example.com/<token>@. Since nobody else knows your bot‘s token, you can be pretty sure it’s us.
setWebhook :: Token
    -> Maybe Text -- ^ HTTPS url to send updates to. Use an empty string to remove webhook integration
    -> Manager
    -> IO (Either ClientError SetWebhookResponse)
setWebhook token url = runClient (setWebhookM request) token
    where request = setWebhookRequest' $ fromMaybe empty url

-- | See 'setWebhook'
setWebhookM :: SetWebhookRequest -> TelegramClient SetWebhookResponse
setWebhookM = run_ setWebhook_

-- | Use this method to specify a url and receive incoming updates via an outgoing webhook. Whenever there is an update for the bot, we will send an HTTPS POST request to the specified url, containing a JSON-serialized 'Update'. In case of an unsuccessful request, we will give up after a reasonable amount of attempts.
--
--       If you'd like to make sure that the Webhook request comes from Telegram, we recommend using a secret path in the URL, e.g. @https://www.example.com/<token>@. Since nobody else knows your bot‘s token, you can be pretty sure it’s us.
setWebhookWithCertificate :: Token -> SetWebhookRequest -> Manager -> IO (Either ClientError SetWebhookResponse)
setWebhookWithCertificate = runM setWebhookWithCertificateM

-- | See 'setWebhookWithCertificate'
setWebhookWithCertificateM :: SetWebhookRequest -> TelegramClient SetWebhookResponse
setWebhookWithCertificateM = run_ setWebhookWithCert_

-- | Use this method to remove webhook integration if you decide to switch back to 'getUpdates'. Returns True on success.
deleteWebhook :: Token -> Manager -> IO (Either ClientError (Response Bool))
deleteWebhook = runClient deleteWebhookM

-- | See 'deleteWebhook'
deleteWebhookM :: TelegramClient (Response Bool)
deleteWebhookM = asking deleteWebhook_

-- | Contains information about the current status of a webhook.
getWebhookInfo :: Token -> Manager -> IO (Either ClientError GetWebhookInfoResponse)
getWebhookInfo = runClient getWebhookInfoM

-- | See 'getWebhookInfo'
getWebhookInfoM :: TelegramClient GetWebhookInfoResponse
getWebhookInfoM = asking getWebhookInfo_
