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
import qualified Data.List.NonEmpty                    as NonEmpty
import qualified Data.Sequence                         as Sequence
import           Data.Text                             (pack)
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
import qualified Servant.Client.Core                   as Core
import           Servant.Client.Internal.HttpClient    (catchConnectionError, clientResponseToResponse,
                                                        requestToClientRequest)
import Data.Binary.Builder (toLazyByteString)
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Lazy as LBS

-- | A type that can be converted to a multipart/form-data value.
class ToMultipartFormData a where
  -- | Convert a Haskell value to a multipart/form-data-friendly intermediate type.
  toMultipartFormData :: a -> [Part]

-- | Extract the request body as a value of type @a@.
data MultipartFormDataReqBody a
    deriving (Typeable)

instance (Core.RunClient m, ToMultipartFormData b, MimeUnrender ct a, cts' ~ (ct ': cts)
  ) => HasClient m (MultipartFormDataReqBody b :> Post cts' a) where
  type Client m (MultipartFormDataReqBody b :> Post cts' a) = b-> ClientM a
  clientWithRoute _pm Proxy req reqData =
    let requestToClientRequest' req' baseurl' = do
          let requestWithoutBody = requestToClientRequest baseurl' req'
          formDataBody (toMultipartFormData reqData) requestWithoutBody
    in snd <$> performRequestCT' requestToClientRequest' (Proxy :: Proxy ct) H.methodPost req

-- copied `performRequest` from servant-0.11, then modified so it takes a variant of `requestToClientRequest`
-- as an argument.
performRequest' :: (Core.Request -> BaseUrl -> IO Request)
               -> Method -> Core.Request
               -> ClientM ( Int, ByteString, MediaType
                          , [HTTP.Header], Client.Response ByteString)
performRequest' requestToClientRequest' reqMethod req = do
  m <- asks manager
  reqHost <- asks baseUrl
  partialRequest <- liftIO $ requestToClientRequest' req reqHost

  let request = partialRequest { Client.method = reqMethod }

  eResponse <- liftIO $ catchConnectionError $ Client.httpLbs request m
  case eResponse of
    Left err ->
      throwError $ Core.ConnectionError $ toException err

    Right response -> do
      let status = Client.responseStatus response
          body = Client.responseBody response
          hdrs = Client.responseHeaders response
          status_code = statusCode status
          coreResponse = clientResponseToResponse id response
      ct <- case lookup "Content-Type" $ Client.responseHeaders response of
                 Nothing -> pure $ "application"//"octet-stream"
                 Just t -> case parseAccept t of
                   Nothing -> throwError $ InvalidContentTypeHeader coreResponse
                   Just t' -> pure t'
      unless (status_code >= 200 && status_code < 300) $
        let builtReq = bimap (const ()) (\b -> (reqHost, LBS.toStrict (toLazyByteString b))) req in
        throwError $ FailureResponse builtReq coreResponse
      return (status_code, body, ct, hdrs, response)

-- copied `performRequestCT` from servant-0.11, then modified so it takes a variant of `requestToClientRequest`
-- as an argument.
performRequestCT' :: MimeUnrender ct result =>
    (Core.Request -> BaseUrl -> IO Request)
    -> Proxy ct -> Method -> Core.Request
    -> ClientM ([HTTP.Header], result)
performRequestCT' requestToClientRequest' ct reqMethod req = do
  let acceptCTS = contentTypes ct
  (_status, respBody, respCT, hdrs, _response) <-
    performRequest' requestToClientRequest' reqMethod (req { Core.requestAccept = Sequence.fromList $ NonEmpty.toList acceptCTS })
  let coreResponse = clientResponseToResponse id _response
  unless (any (matches respCT) acceptCTS) $ throwError $ UnsupportedContentType respCT coreResponse
  case mimeUnrender ct respBody of
    Left err -> throwError $ DecodeFailure (pack err) coreResponse
    Right val -> return (hdrs, val)
