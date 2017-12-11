{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Servant.Client.MultipartFormData
  ( ToMultipartFormData (..)
  , MultipartFormDataReqBody
  ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Data.ByteString.Lazy                  hiding (elem, filter,
                                                        map, null, pack, any)
import           Data.Proxy
import           Data.Foldable (toList)
import           Data.String.Conversions
import           Data.Typeable                         (Typeable)
import           Network.HTTP.Client                   hiding (Proxy, path)
import qualified Network.HTTP.Client                   as Client
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Media
import           Network.HTTP.Types
import qualified Network.HTTP.Types                    as H
import qualified Network.HTTP.Types.Header             as HTTP
import           Servant.API
import           Servant.Client
import           Servant.Common.Req                    (Req, UrlReq (..), ClientEnv (..),
                                                        catchConnectionError,
                                                        reqAccept, reqToRequest)
-- | A type that can be converted to a multipart/form-data value.
class ToMultipartFormData a where
  -- | Convert a Haskell value to a multipart/form-data-friendly intermediate type.
  toMultipartFormData :: a -> [Part]

-- | Extract the request body as a value of type @a@.
data MultipartFormDataReqBody a
    deriving (Typeable)

instance (ToMultipartFormData b, MimeUnrender ct a, cts' ~ (ct ': cts)
  ) => HasClient (MultipartFormDataReqBody b :> Post cts' a) where
  type Client (MultipartFormDataReqBody b :> Post cts' a)
    = b -> ClientM a
  clientWithRoute Proxy req reqData =
    let reqToRequest' req' baseurl' = do
          requestWithoutBody <- reqToRequest req' baseurl'
          formDataBody (toMultipartFormData reqData) requestWithoutBody
    in snd <$> performRequestCT' reqToRequest' (Proxy :: Proxy ct) H.methodPost req

-- copied `performRequest` from servant-0.11, then modified so it takes a variant of `reqToRequest`
-- as an argument.
performRequest' :: (Req -> BaseUrl -> IO Request)
               -> Method -> Req
               -> ClientM ( Int, ByteString, MediaType
                          , [HTTP.Header], Response ByteString)
performRequest' reqToRequest' reqMethod req = do
  m <- asks manager
  reqHost <- asks baseUrl
  partialRequest <- liftIO $ reqToRequest' req reqHost

  let request = partialRequest { Client.method = reqMethod }

  eResponse <- liftIO $ catchConnectionError $ Client.httpLbs request m
  case eResponse of
    Left err ->
      throwError . ConnectionError $ SomeException err

    Right response -> do
      let status = Client.responseStatus response
          body = Client.responseBody response
          hdrs = Client.responseHeaders response
          status_code = statusCode status
      ct <- case lookup "Content-Type" $ Client.responseHeaders response of
                 Nothing -> pure $ "application"//"octet-stream"
                 Just t -> case parseAccept t of
                   Nothing -> throwError $ InvalidContentTypeHeader (cs t) body
                   Just t' -> pure t'
      unless (status_code >= 200 && status_code < 300) $
        throwError $ FailureResponse (UrlReq reqHost req) status ct body
      return (status_code, body, ct, hdrs, response)

-- copied `performRequestCT` from servant-0.11, then modified so it takes a variant of `reqToRequest`
-- as an argument.
performRequestCT' :: MimeUnrender ct result => 
    (Req -> BaseUrl -> IO Request)
    -> Proxy ct -> Method -> Req
    -> ClientM ([HTTP.Header], result)
performRequestCT' reqToRequest' ct reqMethod req = do
  let acceptCTS = contentTypes ct
  (_status, respBody, respCT, hdrs, _response) <-
    performRequest' reqToRequest' reqMethod (req { reqAccept = toList acceptCTS }) 
  unless (any (matches respCT) acceptCTS) $ throwError $ UnsupportedContentType respCT respBody
  case mimeUnrender ct respBody of
    Left err -> throwError $ DecodeFailure err respCT respBody
    Right val -> return (hdrs, val)