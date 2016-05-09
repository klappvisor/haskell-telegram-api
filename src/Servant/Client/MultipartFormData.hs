{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Servant.Client.MultipartFormData
  ( ToMultipartFormData (..)
  , MultipartFormDataReqBody
  ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.ByteString.Lazy       hiding (pack, filter, map, null, elem)
import           Data.Proxy
import           Data.String.Conversions
import           Data.Typeable              (Typeable)
import           Network.HTTP.Client        hiding (Proxy, path)
import qualified Network.HTTP.Client        as Client
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Media
import           Network.HTTP.Types
import qualified Network.HTTP.Types         as H
import qualified Network.HTTP.Types.Header  as HTTP
import           Servant.API
import           Servant.API.Post
import           Servant.Client
import           Servant.Common.Req

-- | A type that can be converted to a multipart/form-data value.
class ToMultipartFormData a where
  -- | Convert a Haskell value to a multipart/form-data-friendly intermediate type.
  toMultipartFormData :: a -> [Part]

-- | Extract the request body as a value of type @a@.
data MultipartFormDataReqBody a
    deriving (Typeable)

instance (ToMultipartFormData b, MimeUnrender ct a)
      => HasClient (MultipartFormDataReqBody b :> Post (ct ': cts) a) where
  type Client (MultipartFormDataReqBody b :> Post (ct ': cts) a)
    = b -> EitherT ServantError IO a
  clientWithRoute Proxy req baseurl reqData =
    let reqToRequest' req' baseurl' = do
          requestWithoutBody <- reqToRequest req' baseurl'
          formDataBody (toMultipartFormData reqData) requestWithoutBody
    in snd <$> performRequestCT' reqToRequest' (Proxy :: Proxy ct) H.methodPost req [200, 201, 202] baseurl

-- copied `performRequest` from servant-0.4.4.7, then modified so it takes a variant of `reqToRequest`
-- as an argument.
performRequest' :: (Req -> BaseUrl -> IO Request)
                -> Method -> Req -> (Int -> Bool) -> BaseUrl
                -> EitherT ServantError IO ( Int, ByteString, MediaType
                                           , [HTTP.Header], Response ByteString)
performRequest' reqToRequest' reqMethod req isWantedStatus reqHost = do
  partialRequest <- liftIO $ reqToRequest' req reqHost

  let request = partialRequest { Client.method = reqMethod
                               , checkStatus = \ _status _headers _cookies -> Nothing
                               }

  eResponse <- liftIO $ __withGlobalManager $ \ manager ->
    catchHttpException $
    Client.httpLbs request manager
  case eResponse of
    Left err ->
      left $ ConnectionError err

    Right response -> do
      let status = Client.responseStatus response
          body = Client.responseBody response
          hrds = Client.responseHeaders response
          status_code = statusCode status
      ct <- case lookup "Content-Type" $ Client.responseHeaders response of
                 Nothing -> pure $ "application"//"octet-stream"
                 Just t -> case parseAccept t of
                   Nothing -> left $ InvalidContentTypeHeader (cs t) body
                   Just t' -> pure t'
      unless (isWantedStatus status_code) $
        left $ FailureResponse status ct body
      return (status_code, body, ct, hrds, response)

-- copied `performRequestCT` from servant-0.4.4.7, then modified so it takes a variant of `reqToRequest`
-- as an argument.
performRequestCT' :: MimeUnrender ct result =>
  (Req -> BaseUrl -> IO Request) ->
  Proxy ct -> Method -> Req -> [Int] -> BaseUrl -> EitherT ServantError IO ([HTTP.Header], result)
performRequestCT' reqToRequest' ct reqMethod req wantedStatus reqHost = do
  let acceptCT = contentType ct
  (_status, respBody, respCT, hrds, _response) <-
    performRequest' reqToRequest' reqMethod (req { reqAccept = [acceptCT] }) (`elem` wantedStatus) reqHost
  unless (matches respCT (acceptCT)) $ left $ UnsupportedContentType respCT respBody
  case mimeUnrender ct respBody of
    Left err -> left $ DecodeFailure err respCT respBody
    Right val -> return (hrds, val)
