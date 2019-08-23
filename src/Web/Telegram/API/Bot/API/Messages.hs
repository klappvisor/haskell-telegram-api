{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Web.Telegram.API.Bot.API.Messages
  ( -- * Functions
    sendMessage
  , sendMessageM
  , forwardMessage
  , forwardMessageM
  , uploadPhoto
  , uploadPhotoM
  , sendPhoto
  , sendPhotoM
  , uploadAudio
  , uploadAudioM
  , sendAudio
  , sendAudioM
  , uploadDocument
  , uploadDocumentM
  , sendDocument
  , sendDocumentM
  , uploadSticker
  , uploadStickerM
  , sendSticker
  , sendStickerM
  , uploadVideo
  , uploadVideoM
  , sendVideo
  , sendVideoM
  , uploadVoice
  , uploadVoiceM
  , sendVoice
  , sendVoiceM
  , uploadVideoNote
  , uploadVideoNoteM
  , sendVideoNote
  , sendVideoNoteM
  , sendMediaGroupM
  , sendLocation
  , sendLocationM
  , sendVenue
  , sendVenueM
  , sendContact
  , sendContactM
  , sendChatAction
  , sendChatActionM
  , sendGame
  , sendGameM
    -- * API
  , TelegramBotMessagesAPI
  , messagesApi
    -- * Types
  ) where

import           Data.Proxy
import           Data.Text                        (Text)
import           Network.HTTP.Client              (Manager)
import           Servant.API
import           Servant.Client            hiding (Response)
import           Servant.Client.MultipartFormData
import           Web.Telegram.API.Bot.API.Core
import           Web.Telegram.API.Bot.Data
import           Web.Telegram.API.Bot.Requests
import           Web.Telegram.API.Bot.Responses

-- | Telegram Bot API
type TelegramBotMessagesAPI =
         TelegramToken :> "sendMessage"
         :> ReqBody '[JSON] SendMessageRequest
         :> Post '[JSON] MessageResponse
    :<|> TelegramToken :> "forwardMessage"
         :> ReqBody '[JSON] ForwardMessageRequest
         :> Post '[JSON] MessageResponse
    :<|> TelegramToken :> "sendPhoto"
         :> MultipartFormDataReqBody (SendPhotoRequest FileUpload)
         :> Post '[JSON] MessageResponse
    :<|> TelegramToken :> "sendPhoto"
         :> ReqBody '[JSON] (SendPhotoRequest Text)
         :> Post '[JSON] MessageResponse
    :<|> TelegramToken :> "sendAudio"
         :> MultipartFormDataReqBody (SendAudioRequest FileUpload)
         :> Post '[JSON] MessageResponse
    :<|> TelegramToken :> "sendAudio"
         :> ReqBody '[JSON] (SendAudioRequest Text)
         :> Post '[JSON] MessageResponse
    :<|> TelegramToken :> "sendDocument"
         :> MultipartFormDataReqBody (SendDocumentRequest FileUpload)
         :> Post '[JSON] MessageResponse
    :<|> TelegramToken :> "sendDocument"
         :> ReqBody '[JSON] (SendDocumentRequest Text)
         :> Post '[JSON] MessageResponse
    :<|> TelegramToken :> "sendSticker"
         :> MultipartFormDataReqBody (SendStickerRequest FileUpload)
         :> Post '[JSON] MessageResponse
    :<|> TelegramToken :> "sendSticker"
         :> ReqBody '[JSON] (SendStickerRequest Text)
         :> Post '[JSON] MessageResponse
    :<|> TelegramToken :> "sendVideo"
         :> MultipartFormDataReqBody (SendVideoRequest FileUpload)
         :> Post '[JSON] MessageResponse
    :<|> TelegramToken :> "sendVideo"
         :> ReqBody '[JSON] (SendVideoRequest Text)
         :> Post '[JSON] MessageResponse
    :<|> TelegramToken :> "sendVoice"
         :> MultipartFormDataReqBody (SendVoiceRequest FileUpload)
         :> Post '[JSON] MessageResponse
    :<|> TelegramToken :> "sendVoice"
         :> ReqBody '[JSON] (SendVoiceRequest Text)
         :> Post '[JSON] MessageResponse
    :<|> TelegramToken :> "sendVideoNote"
         :> MultipartFormDataReqBody (SendVideoNoteRequest FileUpload)
         :> Post '[JSON] MessageResponse
    :<|> TelegramToken :> "sendVideoNote"
         :> ReqBody '[JSON] (SendVideoNoteRequest Text)
         :> Post '[JSON] MessageResponse
    :<|> TelegramToken :> "sendMediaGroup"
         :> ReqBody '[JSON] SendMediaGroupRequest
         :> Post '[JSON] (Response [Message])
    :<|> TelegramToken :> "sendLocation"
         :> ReqBody '[JSON] SendLocationRequest
         :> Post '[JSON] MessageResponse
    :<|> TelegramToken :> "sendVenue"
         :> ReqBody '[JSON] SendVenueRequest
         :> Post '[JSON] MessageResponse
    :<|> TelegramToken :> "sendContact"
         :> ReqBody '[JSON] SendContactRequest
         :> Post '[JSON] MessageResponse
    :<|> TelegramToken :> "sendChatAction"
         :> ReqBody '[JSON] SendChatActionRequest
         :> Post '[JSON] ChatActionResponse
    :<|> TelegramToken :> "sendGame"
         :> ReqBody '[JSON] SendGameRequest
         :> Post '[JSON] MessageResponse

-- | Proxy for Thelegram Bot API
messagesApi :: Proxy TelegramBotMessagesAPI
messagesApi = Proxy

sendMessage_               :: Token -> SendMessageRequest -> ClientM MessageResponse
forwardMessage_            :: Token -> ForwardMessageRequest -> ClientM MessageResponse
uploadPhoto_               :: Token -> SendPhotoRequest FileUpload -> ClientM MessageResponse
sendPhoto_                 :: Token -> SendPhotoRequest Text -> ClientM MessageResponse
uploadAudio_               :: Token -> SendAudioRequest FileUpload -> ClientM MessageResponse
sendAudio_                 :: Token -> SendAudioRequest Text -> ClientM MessageResponse
uploadDocument_            :: Token -> SendDocumentRequest FileUpload -> ClientM MessageResponse
sendDocument_              :: Token -> SendDocumentRequest Text -> ClientM MessageResponse
uploadSticker_             :: Token -> SendStickerRequest FileUpload -> ClientM MessageResponse
sendSticker_               :: Token -> SendStickerRequest Text -> ClientM MessageResponse
uploadVideo_               :: Token -> SendVideoRequest FileUpload -> ClientM MessageResponse
sendVideo_                 :: Token -> SendVideoRequest Text -> ClientM MessageResponse
uploadVoice_               :: Token -> SendVoiceRequest FileUpload -> ClientM MessageResponse
sendVoice_                 :: Token -> SendVoiceRequest Text -> ClientM MessageResponse
uploadVideoNote_           :: Token -> SendVideoNoteRequest FileUpload -> ClientM MessageResponse
sendVideoNote_             :: Token -> SendVideoNoteRequest Text -> ClientM MessageResponse
sendMediaGroup_            :: Token -> SendMediaGroupRequest -> ClientM (Response [Message])
sendLocation_              :: Token -> SendLocationRequest -> ClientM MessageResponse
sendVenue_                 :: Token -> SendVenueRequest-> ClientM MessageResponse
sendContact_               :: Token -> SendContactRequest -> ClientM MessageResponse
sendChatAction_            :: Token -> SendChatActionRequest -> ClientM ChatActionResponse
sendGame_                  :: Token -> SendGameRequest -> ClientM MessageResponse
sendMessage_
  :<|> forwardMessage_
  :<|> uploadPhoto_
  :<|> sendPhoto_
  :<|> uploadAudio_
  :<|> sendAudio_
  :<|> uploadDocument_
  :<|> sendDocument_
  :<|> uploadSticker_
  :<|> sendSticker_
  :<|> uploadVideo_
  :<|> sendVideo_
  :<|> uploadVoice_
  :<|> sendVoice_
  :<|> uploadVideoNote_
  :<|> sendVideoNote_
  :<|> sendMediaGroup_
  :<|> sendLocation_
  :<|> sendVenue_
  :<|> sendContact_
  :<|> sendChatAction_
  :<|> sendGame_
     = client messagesApi

-- | Use this method to send text messages. On success, the sent 'Message' is returned.
sendMessage :: Token -> SendMessageRequest -> Manager -> IO (Either ClientError MessageResponse)
sendMessage = runM sendMessageM

-- | See 'sendMessage'
sendMessageM :: SendMessageRequest -> TelegramClient MessageResponse
sendMessageM = run_ sendMessage_

-- | Use this method to forward messages of any kind. On success, the sent 'Message' is returned.
forwardMessage :: Token -> ForwardMessageRequest -> Manager -> IO (Either ClientError MessageResponse)
forwardMessage = runM forwardMessageM

-- | See 'forwardMessage'
forwardMessageM :: ForwardMessageRequest -> TelegramClient MessageResponse
forwardMessageM = run_ forwardMessage_

-- | Use this method to upload and send photos. On success, the sent 'Message' is returned.
uploadPhoto :: Token -> SendPhotoRequest FileUpload -> Manager -> IO (Either ClientError MessageResponse)
uploadPhoto = runM uploadPhotoM

-- | See 'uploadPhoto'
uploadPhotoM :: SendPhotoRequest FileUpload -> TelegramClient MessageResponse
uploadPhotoM = run_ uploadPhoto_

-- | Use this method to send photos that have already been uploaded. On success, the sent 'Message' is returned.
sendPhoto :: Token -> SendPhotoRequest Text -> Manager -> IO (Either ClientError MessageResponse)
sendPhoto = runM sendPhotoM

-- | See 'sendPhoto'
sendPhotoM :: SendPhotoRequest Text -> TelegramClient MessageResponse
sendPhotoM = run_ sendPhoto_

-- | Use this method to upload and send audio files, if you want Telegram clients to display them in the music player. Your audio must be in the .mp3 format. On success, the sent 'Message' is returned. Bots can currently send audio files of up to 50 MB in size, this limit may be changed in the future.
--
--       For backward compatibility, when the fields __title__ and __performer__ are both empty and the mime-type of the file to be sent is not _audio/mpeg_, the file will be sent as a playable voice message. For this to work, the audio must be in an .ogg file encoded with OPUS. This behavior will be phased out in the future. For sending voice messages, use the 'sendVoice' method instead.
uploadAudio :: Token -> SendAudioRequest FileUpload -> Manager -> IO (Either ClientError MessageResponse)
uploadAudio = runM uploadAudioM

-- | See 'uploadAudio'
uploadAudioM :: SendAudioRequest FileUpload -> TelegramClient MessageResponse
uploadAudioM = run_ uploadAudio_

-- | Use this method to send audio files that are already on the Telegram servers, if you want Telegram clients to display them in the music player. Your audio must be in the .mp3 format. On success, the sent 'Message' is returned. Bots can currently send audio files of up to 50 MB in size, this limit may be changed in the future.
--
--       For backward compatibility, when the fields __title__ and __performer__ are both empty and the mime-type of the file to be sent is not _audio/mpeg_, the file will be sent as a playable voice message. For this to work, the audio must be in an .ogg file encoded with OPUS. This behavior will be phased out in the future. For sending voice messages, use the 'sendVoice' method instead.
sendAudio :: Token -> SendAudioRequest Text -> Manager -> IO (Either ClientError MessageResponse)
sendAudio = runM sendAudioM

-- | See 'sendAudio'
sendAudioM :: SendAudioRequest Text -> TelegramClient MessageResponse
sendAudioM = run_ sendAudio_

-- | Use this method to upload and send general files. On success, the sent 'Message' is returned. Bots can currently send files of any type of up to 50 MB in size, this limit may be changed in the future.
uploadDocument :: Token -> SendDocumentRequest FileUpload -> Manager -> IO (Either ClientError MessageResponse)
uploadDocument = runM uploadDocumentM

-- | See 'uploadDocument'
uploadDocumentM :: SendDocumentRequest FileUpload -> TelegramClient MessageResponse
uploadDocumentM = run_ uploadDocument_

-- | Use this method to send general files that have already been uploaded. On success, the sent 'Message' is returned. Bots can currently send files of any type of up to 50 MB in size, this limit may be changed in the future.
sendDocument :: Token -> SendDocumentRequest Text -> Manager -> IO (Either ClientError MessageResponse)
sendDocument = runM sendDocumentM

-- | See 'sendDocument'
sendDocumentM :: SendDocumentRequest Text -> TelegramClient MessageResponse
sendDocumentM = run_ sendDocument_

-- | Use this method to upload and send .webp stickers. On success, the sent 'Message' is returned.
uploadSticker :: Token -> SendStickerRequest FileUpload -> Manager -> IO (Either ClientError MessageResponse)
uploadSticker = runM uploadStickerM

-- | See 'uploadSticker'
uploadStickerM :: SendStickerRequest FileUpload -> TelegramClient MessageResponse
uploadStickerM = run_ uploadSticker_

-- | Use this method to send .webp stickers that are already on the Telegram servers. On success, the sent 'Message' is returned.
sendSticker :: Token -> SendStickerRequest Text -> Manager -> IO (Either ClientError MessageResponse)
sendSticker = runM sendStickerM

-- | See 'sendSticker'
sendStickerM :: SendStickerRequest Text -> TelegramClient MessageResponse
sendStickerM = run_ sendSticker_

-- | Use this method to upload and send video files. Telegram clients support mp4 videos (other formats may be sent as 'Document'). On success, the sent 'Message' is returned. Bots can currently send video files of up to 50 MB in size, this limit may be changed in the future.
uploadVideo :: Token -> SendVideoRequest FileUpload -> Manager -> IO (Either ClientError MessageResponse)
uploadVideo = runM uploadVideoM

-- | See 'uploadVideo'
uploadVideoM :: SendVideoRequest FileUpload -> TelegramClient MessageResponse
uploadVideoM = run_ uploadVideo_

-- | Use this method to send video files that are already on the Telegram servers. Telegram clients support mp4 videos (other formats may be sent as 'Document'). On success, the sent 'Message' is returned. Bots can currently send video files of up to 50 MB in size, this limit may be changed in the future.
sendVideo :: Token -> SendVideoRequest Text -> Manager -> IO (Either ClientError MessageResponse)
sendVideo = runM sendVideoM

-- | See 'sendVideo'
sendVideoM :: SendVideoRequest Text -> TelegramClient MessageResponse
sendVideoM = run_ sendVideo_

-- | Use this method to upload and send audio files, if you want Telegram clients to display the file as a playable voice message. For this to work, your audio must be in an .ogg file encoded with OPUS (other formats may be sent as 'Audio' or 'Document'). On success, the sent 'Message' is returned. Bots can currently send voice messages of up to 50 MB in size, this limit may be changed in the future.
uploadVoice :: Token -> SendVoiceRequest FileUpload -> Manager -> IO (Either ClientError MessageResponse)
uploadVoice = runM uploadVoiceM

-- | See 'uploadVoice'
uploadVoiceM :: SendVoiceRequest FileUpload -> TelegramClient MessageResponse
uploadVoiceM = run_ uploadVoice_

-- | Use this method to send audio files that are already on the telegram server, if you want Telegram clients to display the file as a playable voice message. For this to work, your audio must be in an .ogg file encoded with OPUS (other formats may be sent as 'Audio' or 'Document'). On success, the sent 'Message' is returned. Bots can currently send voice messages of up to 50 MB in size, this limit may be changed in the future.
sendVoice :: Token -> SendVoiceRequest Text -> Manager -> IO (Either ClientError MessageResponse)
sendVoice = runM sendVoiceM

-- | See 'sendVoice'
sendVoiceM :: SendVoiceRequest Text -> TelegramClient MessageResponse
sendVoiceM = run_ sendVoice_

-- | As of v.4.0, Telegram clients support rounded square mp4 videos of up to 1 minute long. Use this method to send video messages. On success, the sent Message is returned.
uploadVideoNote :: Token -> SendVideoNoteRequest FileUpload -> Manager -> IO (Either ClientError MessageResponse)
uploadVideoNote = runM uploadVideoNoteM

-- | See 'uploadVideoNote'
uploadVideoNoteM :: SendVideoNoteRequest FileUpload -> TelegramClient MessageResponse
uploadVideoNoteM = run_ uploadVideoNote_

-- | As of v.4.0, Telegram clients support rounded square mp4 videos of up to 1 minute long. Use this method to send video messages. On success, the sent Message is returned.
sendVideoNote :: Token -> SendVideoNoteRequest Text -> Manager -> IO (Either ClientError MessageResponse)
sendVideoNote = runM sendVideoNoteM

-- | See 'sendVoice'
sendVideoNoteM :: SendVideoNoteRequest Text -> TelegramClient MessageResponse
sendVideoNoteM = run_ sendVideoNote_

-- | Use this method to send point on the map. On success, the sent 'Message' is returned.
sendLocation :: Token -> SendLocationRequest -> Manager -> IO (Either ClientError MessageResponse)
sendLocation = runM sendLocationM

-- | See 'sendLocation'
sendLocationM :: SendLocationRequest -> TelegramClient MessageResponse
sendLocationM = run_ sendLocation_

sendMediaGroupM :: SendMediaGroupRequest -> TelegramClient (Response [Message])
sendMediaGroupM = run_ sendMediaGroup_

-- | Use this method to send information about a venue. On success, the sent 'Message' is returned.
sendVenue :: Token -> SendVenueRequest -> Manager -> IO (Either ClientError MessageResponse)
sendVenue = runM sendVenueM

-- | See 'sendVenue'
sendVenueM :: SendVenueRequest -> TelegramClient MessageResponse
sendVenueM = run_ sendVenue_

-- | Use this method to send information about a venue. On success, the sent 'Message' is returned.
sendContact :: Token -> SendContactRequest -> Manager -> IO (Either ClientError MessageResponse)
sendContact = runM sendContactM

-- | See 'sendContact'
sendContactM :: SendContactRequest -> TelegramClient MessageResponse
sendContactM = run_ sendContact_

-- | Use this method when you need to tell the user that something is happening on the bot's side.
--   The status is set for 5 seconds or less (when a message arrives from your bot,
--   Telegram clients clear its typing status).
sendChatAction :: Token -> SendChatActionRequest -> Manager -> IO (Either ClientError ChatActionResponse)
sendChatAction = runM sendChatActionM

-- | See 'sendChatAction'
sendChatActionM :: SendChatActionRequest -> TelegramClient ChatActionResponse
sendChatActionM = run_ sendChatAction_

-- | Use this method to send a game. On success, the sent 'Message' is returned.
sendGame :: Token -> SendGameRequest -> Manager -> IO (Either ClientError MessageResponse)
sendGame = runM sendGameM

-- | See 'sendGame'
sendGameM :: SendGameRequest -> TelegramClient MessageResponse
sendGameM = run_ sendGame_
