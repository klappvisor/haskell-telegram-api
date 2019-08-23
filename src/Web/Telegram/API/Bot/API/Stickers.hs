{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Web.Telegram.API.Bot.API.Stickers
  ( -- * Functions
    getStickerSetM
  , uploadStickerFileM
  , createNewStickerSetM
  , createNewStickerSetM'
  , addStickerToSetM
  , uploadStickerToSetM
  , setStickerPositionInSetM
  , deleteStickerFromSetM
    -- * API
  , TelegramBotStickersAPI
  , stickerApi
    -- * Types
  ) where

import           Data.Proxy
import           Data.Text                        (Text)
import           Servant.API
import           Servant.Client            hiding (Response)
import           Servant.Client.MultipartFormData
import           Web.Telegram.API.Bot.API.Core
import           Web.Telegram.API.Bot.Data
import           Web.Telegram.API.Bot.Requests

import           Web.Telegram.API.Bot.Responses   (Response)

-- | Telegram Bot API
type TelegramBotStickersAPI =
         TelegramToken :> "getStickerSet"
         :> QueryParam "name" Text
         :> Get '[JSON] (Response StickerSet)
    :<|> TelegramToken :> "uploadStickerFile"
         :> MultipartFormDataReqBody UploadStickerFileRequest
         :> Post '[JSON] (Response File)
    :<|> TelegramToken :> "createNewStickerSet"
         :> ReqBody '[JSON] (CreateNewStickerSetRequest Text)
         :> Post '[JSON] (Response Bool)
    :<|> TelegramToken :> "createNewStickerSet"
         :> MultipartFormDataReqBody (CreateNewStickerSetRequest FileUpload)
         :> Post '[JSON] (Response Bool)
    :<|> TelegramToken :> "addStickerToSet"
         :> ReqBody '[JSON] (AddStickerToSetRequest Text)
         :> Post '[JSON] (Response Bool)
    :<|> TelegramToken :> "addStickerToSet"
         :> MultipartFormDataReqBody (AddStickerToSetRequest FileUpload)
         :> Post '[JSON] (Response Bool)
    :<|> TelegramToken :> "setStickerPositionInSet"
         :> QueryParam "sticker" Text
         :> QueryParam "position" Int
         :> Post '[JSON] (Response Bool)
    :<|> TelegramToken :> "deleteStickerFromSet"
         :> QueryParam "sticker" Text
         :> Post '[JSON] (Response Bool)

-- | Proxy for Thelegram Bot API
stickerApi :: Proxy TelegramBotStickersAPI
stickerApi = Proxy

getStickerSet_           :: Token -> Maybe Text -> ClientM (Response StickerSet)
uploadStickerFile_       :: Token -> UploadStickerFileRequest -> ClientM (Response File)
createNewStickerSet_     :: Token -> CreateNewStickerSetRequest Text -> ClientM (Response Bool)
createNewStickerSet_'    :: Token -> CreateNewStickerSetRequest FileUpload -> ClientM (Response Bool)
addStickerToSet_         :: Token -> AddStickerToSetRequest Text -> ClientM (Response Bool)
addStickerToSet_'        :: Token -> AddStickerToSetRequest FileUpload -> ClientM (Response Bool)
setStickerPositionInSet_ :: Token -> Maybe Text -> Maybe Int -> ClientM (Response Bool)
deleteStickerFromSet_    :: Token -> Maybe Text -> ClientM (Response Bool)
getStickerSet_
  :<|> uploadStickerFile_
  :<|> createNewStickerSet_
  :<|> createNewStickerSet_'
  :<|> addStickerToSet_
  :<|> addStickerToSet_'
  :<|> setStickerPositionInSet_
  :<|> deleteStickerFromSet_
     = client stickerApi

-- | Use this method to get a sticker set.
getStickerSetM :: Text -- ^ Name of the sticker set
               -> TelegramClient (Response StickerSet)
getStickerSetM name = run_ getStickerSet_ $ Just name

-- | Use this method to upload a .png file with a sticker for later use in 'createNewStickerSet' and 'addStickerToSet' methods (can be used multiple times).
uploadStickerFileM :: UploadStickerFileRequest -> TelegramClient (Response File)
uploadStickerFileM = run_ uploadStickerFile_

-- | Use this method to create new sticker set owned by a user. The bot will be able to edit the created sticker set.
createNewStickerSetM :: CreateNewStickerSetRequest Text -> TelegramClient (Response Bool)
createNewStickerSetM = run_ createNewStickerSet_

createNewStickerSetM' :: CreateNewStickerSetRequest FileUpload -> TelegramClient (Response Bool)
createNewStickerSetM' = run_ createNewStickerSet_'

-- | Use this method to add a new sticker to a set created by the bot.
addStickerToSetM :: AddStickerToSetRequest Text -> TelegramClient (Response Bool)
addStickerToSetM = run_ addStickerToSet_

uploadStickerToSetM :: AddStickerToSetRequest FileUpload -> TelegramClient (Response Bool)
uploadStickerToSetM = run_ addStickerToSet_'

-- | Use this method to move a sticker in a set created by the bot to a specific position.
setStickerPositionInSetM :: Text -- ^ File identifier of the sticker
                         -> Int -- ^ New sticker position in the set, zero-based
                         -> TelegramClient (Response Bool)
setStickerPositionInSetM fileId position = asking $ \t -> setStickerPositionInSet_ t (Just fileId) (Just position)

-- | Use this method to delete a sticker from a set created by the bot.
deleteStickerFromSetM :: Text -- ^ File identifier of the sticker
                      -> TelegramClient (Response Bool)
deleteStickerFromSetM fileId = run_ deleteStickerFromSet_ (Just fileId)
