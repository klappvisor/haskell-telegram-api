{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Web.Telegram.API.Bot.API.Payments
  ( -- * Functions
    sendInvoiceM
  , answerShippingQueryM
  , answerPreCheckoutQueryM
    -- * API
  , TelegramBotPaymentsAPI
  , paymentsApi
    -- * Types
  ) where

import           Data.Proxy
import           Servant.API
import           Servant.Client
import           Web.Telegram.API.Bot.API.Core
import           Web.Telegram.API.Bot.Requests
import           Web.Telegram.API.Bot.Responses

type TelegramBotPaymentsAPI =
        TelegramToken :> "sendInvoice"
        :> ReqBody '[JSON] SendInvoiceRequest
        :> Post '[JSON] MessageResponse
   :<|> TelegramToken :> "answerShippingQuery"
        :> ReqBody '[JSON] AnswerShippingQueryRequest
        :> Post '[JSON] AnswerShippingQueryResponse
   :<|> TelegramToken :> "answerPreCheckoutQuery"
        :> ReqBody '[JSON] AnswerPreCheckoutQueryRequest
        :> Post '[JSON] AnswerPreCheckoutQueryResponse

paymentsApi :: Proxy TelegramBotPaymentsAPI
paymentsApi = Proxy

sendInvoice_ :: Token -> SendInvoiceRequest -> ClientM MessageResponse
answerShippingQuery_ :: Token -> AnswerShippingQueryRequest -> ClientM AnswerShippingQueryResponse
answerPreCheckoutQuery_ :: Token -> AnswerPreCheckoutQueryRequest -> ClientM AnswerPreCheckoutQueryResponse
sendInvoice_
  :<|> answerShippingQuery_
  :<|> answerPreCheckoutQuery_
     = client paymentsApi

sendInvoiceM :: SendInvoiceRequest -> TelegramClient MessageResponse
sendInvoiceM = run_ sendInvoice_

answerShippingQueryM :: AnswerShippingQueryRequest -> TelegramClient AnswerShippingQueryResponse
answerShippingQueryM = run_ answerShippingQuery_

answerPreCheckoutQueryM :: AnswerPreCheckoutQueryRequest -> TelegramClient AnswerPreCheckoutQueryResponse
answerPreCheckoutQueryM = run_ answerPreCheckoutQuery_
