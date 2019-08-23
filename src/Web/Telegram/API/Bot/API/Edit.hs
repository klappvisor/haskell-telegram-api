{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Web.Telegram.API.Bot.API.Edit
  ( -- * Functions
    editMessageText
  , editMessageTextM
  , editMessageCaption
  , editMessageCaptionM
  , editMessageReplyMarkup
  , editMessageReplyMarkupM
  , editInlineMessageText
  , editInlineMessageTextM
  , editInlineMessageCaption
  , editInlineMessageCaptionM
  , editInlineMessageReplyMarkup
  , editInlineMessageReplyMarkupM
  , editMessageLiveLocationM
  , stopMessageLiveLocationM
    -- * API
  , TelegramBotEditAPI
  , editApi
    -- * Types
  ) where

import           Data.Proxy
import           Network.HTTP.Client            (Manager)
import           Servant.API
import           Servant.Client          hiding (Response)
import           Web.Telegram.API.Bot.API.Core
import           Web.Telegram.API.Bot.Requests
import           Web.Telegram.API.Bot.Responses

-- | Telegram Bot API
type TelegramBotEditAPI =
         TelegramToken :> "editMessageText"
         :> ReqBody '[JSON] EditMessageTextRequest
         :> Post '[JSON] MessageResponse
    :<|> TelegramToken :> "editMessageCaption"
         :> ReqBody '[JSON] EditMessageCaptionRequest
         :> Post '[JSON] MessageResponse
    :<|> TelegramToken :> "editMessageReplyMarkup"
         :> ReqBody '[JSON] EditMessageReplyMarkupRequest
         :> Post '[JSON] MessageResponse
    :<|> TelegramToken :> "editMessageText"
         :> ReqBody '[JSON] EditMessageTextRequest
         :> Post '[JSON] (Response Bool)
    :<|> TelegramToken :> "editMessageCaption"
         :> ReqBody '[JSON] EditMessageCaptionRequest
         :> Post '[JSON] (Response Bool)
    :<|> TelegramToken :> "editMessageReplyMarkup"
         :> ReqBody '[JSON] EditMessageReplyMarkupRequest
         :> Post '[JSON] (Response Bool)
    :<|> TelegramToken :> "editMessageLiveLocation"
         :> ReqBody '[JSON] EditMessageLiveLocationRequest
         :> Post '[JSON] (Response Bool)
    :<|> TelegramToken :> "stopMessageLiveLocation"
         :> ReqBody '[JSON] StopMessageLiveLocationRequest
         :> Post '[JSON] (Response Bool)


-- | Proxy for Thelegram Bot API
editApi :: Proxy TelegramBotEditAPI
editApi = Proxy

editMessageText_           :: Token -> EditMessageTextRequest -> ClientM MessageResponse
editMessageCaption_        :: Token -> EditMessageCaptionRequest -> ClientM MessageResponse
editMessageReplyMarkup_    :: Token -> EditMessageReplyMarkupRequest -> ClientM MessageResponse
editMessageText__          :: Token -> EditMessageTextRequest -> ClientM (Response Bool)
editMessageCaption__       :: Token -> EditMessageCaptionRequest -> ClientM (Response Bool)
editMessageReplyMarkup__   :: Token -> EditMessageReplyMarkupRequest -> ClientM (Response Bool)
editMessageLiveLocation_   :: Token -> EditMessageLiveLocationRequest -> ClientM (Response Bool)
stopMessageLiveLocation_   :: Token -> StopMessageLiveLocationRequest -> ClientM (Response Bool)
editMessageText_
  :<|> editMessageCaption_
  :<|> editMessageReplyMarkup_
  :<|> editMessageText__
  :<|> editMessageCaption__
  :<|> editMessageReplyMarkup__
  :<|> editMessageLiveLocation_
  :<|> stopMessageLiveLocation_
     = client editApi

-- | Use this method to edit text messages sent by the bot. On success, the edited 'Message' is returned, otherwise True is returned.
editMessageText :: Token -> EditMessageTextRequest -> Manager -> IO (Either ClientError MessageResponse)
editMessageText = runM editMessageTextM

-- | See 'editMessageText'
editMessageTextM :: EditMessageTextRequest -> TelegramClient MessageResponse
editMessageTextM = run_ editMessageText_

-- | Use this method to edit captions of messages sent by the bot. On success, the edited 'Message' is returned.
editMessageCaption :: Token -> EditMessageCaptionRequest -> Manager -> IO (Either ClientError MessageResponse)
editMessageCaption = runM editMessageCaptionM

-- | See 'editMessageCaption'
editMessageCaptionM :: EditMessageCaptionRequest -> TelegramClient MessageResponse
editMessageCaptionM = run_ editMessageCaption_

-- | Use this method to edit only the reply markup of messages sent by the bot. On success, the edited 'Message' is returned.
editMessageReplyMarkup :: Token -> EditMessageReplyMarkupRequest -> Manager -> IO (Either ClientError MessageResponse)
editMessageReplyMarkup = runM editMessageReplyMarkupM

editMessageReplyMarkupM :: EditMessageReplyMarkupRequest -> TelegramClient MessageResponse
editMessageReplyMarkupM = run_ editMessageReplyMarkup_

-- | Use this method to edit text messages sent via the bot (for inline bots).
editInlineMessageText :: Token -> EditMessageTextRequest -> Manager -> IO (Either ClientError (Response Bool))
editInlineMessageText = runM editInlineMessageTextM

-- | See 'editInlineMessageText'
editInlineMessageTextM :: EditMessageTextRequest -> TelegramClient (Response Bool)
editInlineMessageTextM = run_ editMessageText__

-- | Use this method to edit captions of messages sent via the bot (for inline bots).
editInlineMessageCaption :: Token -> EditMessageCaptionRequest -> Manager -> IO (Either ClientError (Response Bool))
editInlineMessageCaption = runM editInlineMessageCaptionM

-- | See 'editInlineMessageCaption'
editInlineMessageCaptionM :: EditMessageCaptionRequest -> TelegramClient (Response Bool)
editInlineMessageCaptionM = run_ editMessageCaption__

-- | Use this method to edit only the reply markup of messages sent via the bot (for inline bots).
editInlineMessageReplyMarkup :: Token -> EditMessageReplyMarkupRequest -> Manager -> IO (Either ClientError (Response Bool))
editInlineMessageReplyMarkup = runM editInlineMessageReplyMarkupM

-- | See 'editInlineMessageReplyMarkup'
editInlineMessageReplyMarkupM :: EditMessageReplyMarkupRequest -> TelegramClient (Response Bool)
editInlineMessageReplyMarkupM = run_ editMessageReplyMarkup__

-- | Use this method to edit live location messages sent by the bot or via the bot (for inline bots). A location can be edited until its live_period expires or editing is explicitly disabled by a call to 'stopMessageLiveLocationM'.
editMessageLiveLocationM :: EditMessageLiveLocationRequest -> TelegramClient (Response Bool)
editMessageLiveLocationM = run_ editMessageLiveLocation_

-- | Use this method to stop updating a live location message sent by the bot or via the bot (for inline bots) before live_period expires.
stopMessageLiveLocationM :: StopMessageLiveLocationRequest -> TelegramClient (Response Bool)
stopMessageLiveLocationM = run_ stopMessageLiveLocation_
