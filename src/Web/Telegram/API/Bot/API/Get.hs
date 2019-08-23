{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Web.Telegram.API.Bot.API.Get
  ( -- * Functions
    getMe
  , getMeM
  , getFile
  , getFileM
  , getUserProfilePhotos
  , getUserProfilePhotosM
    -- * API
  , TelegramBotGetAPI
  , getApi
    -- * Types
  ) where

import           Data.Proxy
import           Data.Text                      (Text)
import           Network.HTTP.Client            (Manager)
import           Servant.API
import           Servant.Client
import           Web.Telegram.API.Bot.API.Core
import           Web.Telegram.API.Bot.Responses

-- | Telegram Bot API
type TelegramBotGetAPI =
         TelegramToken :> "getMe"
         :> Get '[JSON] GetMeResponse
    :<|> TelegramToken :> "getFile"
         :> QueryParam "file_id" Text
         :> Get '[JSON] FileResponse
    :<|> TelegramToken :> "getUserProfilePhotos"
         :> QueryParam "user_id" Integer
         :> QueryParam "offset" Int
         :> QueryParam "limit" Int
         :> Get '[JSON] UserProfilePhotosResponse

-- | Proxy for Thelegram Bot API
getApi :: Proxy TelegramBotGetAPI
getApi = Proxy

getMe_                     :: Token -> ClientM GetMeResponse
getFile_                   :: Token -> Maybe Text -> ClientM FileResponse
getUserProfilePhotos_      :: Token -> Maybe Integer -> Maybe Int -> Maybe Int -> ClientM UserProfilePhotosResponse
getMe_
  :<|> getFile_
  :<|> getUserProfilePhotos_
     = client getApi

-- | A simple method for testing your bot's auth token. Requires no parameters.
--   Returns basic information about the bot in form of a 'User' object.
getMe :: Token -> Manager -> IO (Either ClientError GetMeResponse)
getMe = runClient getMeM

-- | See `getMe`
getMeM :: TelegramClient GetMeResponse
getMeM = asking getMe_

-- | Use this method to get basic info about a file and prepare it for downloading. For the moment, bots can download files of up to 20MB in size. On success, a 'File' object is returned. The file can then be downloaded via the link @https://api.telegram.org/file/bot<token>/<file_path>@, where @<file_path>@ is taken from the response. It is guaranteed that the link will be valid for at least 1 hour. When the link expires, a new one can be requested by calling getFile again.
getFile :: Token -> Text -> Manager -> IO (Either ClientError FileResponse)
getFile token fileId = runClient (getFileM fileId) token

-- | See 'getFile'
getFileM :: Text -> TelegramClient FileResponse
getFileM fileId = run_ getFile_ (Just fileId)

-- | Use this method to get a list of profile pictures for a user. Returns a 'UserProfilePhotos' object.
getUserProfilePhotos :: Token -> Integer -> Maybe Int -> Maybe Int -> Manager -> IO (Either ClientError UserProfilePhotosResponse)
getUserProfilePhotos token userId offset limit = runClient (getUserProfilePhotosM userId offset limit) token

-- | See 'getUserProfilePhotos'
getUserProfilePhotosM :: Integer -> Maybe Int -> Maybe Int -> TelegramClient UserProfilePhotosResponse
getUserProfilePhotosM userId offset limit = asking $ \t -> getUserProfilePhotos_ t (Just userId) offset limit
