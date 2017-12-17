{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module PaymentsSpec (spec) where

import           Data.Text               (Text)
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Test.Hspec
import           TestCore
import           Web.Telegram.API.Bot

spec :: Token -> ChatId -> Text -> Text -> Spec
spec token chatId' _ paymentToken = do
  manager <- runIO $ newManager tlsManagerSettings
  let ChatId chatId = chatId'
  describe "/sendInvoice" $ do
    it "should send invoice" $ do
      let description = "The best portal cannon in known universe"
          currencyCode = CurrencyCode "USD"
          prices = [ LabeledPrice "Cannon price" 59999
                   , LabeledPrice "Time-space penetration tax" 2100
                   , LabeledPrice "Portal Invention Day discount" (-1200)
                   , LabeledPrice "Portal delivery cost" 100
                   ]
          invoiceRequest = (sendInvoiceRequest chatId "Portal cannon" description "test_payload" paymentToken "deep_link" currencyCode prices)
                   { snd_inv_photo_url = Just "http://farm4.staticflickr.com/3560/3576111171_66c1fc2462_z.jpg"
                   , snd_inv_is_flexible = Just True
                   }
      res <- runClient (sendInvoiceM invoiceRequest) token manager
      success res
      let Right Response { result = m } = res
      inv_title <$> (invoice m) `shouldBe` Just "Portal cannon"

