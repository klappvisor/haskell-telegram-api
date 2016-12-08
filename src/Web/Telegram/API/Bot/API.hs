{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Web.Telegram.API.Bot.API
  ( -- * Functions
    getMe
  , sendMessage
  , forwardMessage
  , uploadPhoto
  , sendPhoto
  , uploadAudio
  , sendAudio
  , uploadDocument
  , sendDocument
  , uploadSticker
  , sendSticker
  , uploadVideo
  , sendVideo
  , uploadVoice
  , sendVoice
  , sendLocation
  , sendVenue
  , sendContact
  , sendChatAction
  , getUpdates
  , getFile
  , getUserProfilePhotos
  , setWebhook
  , setWebhookWithCertificate
  , answerInlineQuery
  , answerCallbackQuery
  , kickChatMember
  , leaveChat
  , unbanChatMember
  , getChat
  , getChatAdministrators
  , getChatMembersCount
  , getChatMember
  , editMessageText
  , editMessageCaption
  , editMessageReplyMarkup
  , editInlineMessageText
  , editInlineMessageCaption
  , editInlineMessageReplyMarkup
    -- * API
  , TelegramBotAPI
  , api
    -- * Types
  , Token             (..)
  ) where

import           Data.Proxy
import           Data.Text (Text)
import           Network.HTTP.Client (Manager)
import           Servant.API
import           Servant.Client
import           Servant.Client.MultipartFormData
import           Web.Telegram.API.Bot.Responses
import           Web.Telegram.API.Bot.Requests

-- | Telegram Bot's Token
newtype Token = Token Text
  deriving (Show, Eq, Ord, ToHttpApiData, FromHttpApiData)

telegramBaseUrl :: BaseUrl
telegramBaseUrl = BaseUrl Https "api.telegram.org" 443 ""

-- | Type for token
type TelegramToken = Capture ":token" Token

-- | Telegram Bot API
type TelegramBotAPI =
         TelegramToken :> "getMe"
         :> Get '[JSON] GetMeResponse
    :<|> TelegramToken :> "sendMessage"
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
    :<|> TelegramToken :> "getUpdates"
         :> QueryParam "offset" Int
         :> QueryParam "limit" Int
         :> QueryParam "timeout" Int
         :> Get '[JSON] UpdatesResponse
    :<|> TelegramToken :> "getFile"
         :> QueryParam "file_id" Text
         :> Get '[JSON] FileResponse
    :<|> TelegramToken :> "getUserProfilePhotos"
         :> QueryParam "user_id" Int
         :> QueryParam "offset" Int
         :> QueryParam "limit" Int
         :> Get '[JSON] UserProfilePhotosResponse
    :<|> TelegramToken :> "setWebhook"
         :> QueryParam "url" Text
         :> Get '[JSON] SetWebhookResponse
    :<|> TelegramToken :> "setWebhook"
         :> MultipartFormDataReqBody SetWebhookRequest
         :> Post '[JSON] SetWebhookResponse
    :<|> TelegramToken :> "answerInlineQuery"
         :> ReqBody '[JSON] AnswerInlineQueryRequest
         :> Post '[JSON] InlineQueryResponse
    :<|> TelegramToken :> "answerCallbackQuery"
         :> ReqBody '[JSON] AnswerCallbackQueryRequest
         :> Post '[JSON] CallbackQueryResponse
    :<|> TelegramToken :> "kickChatMember"
         :> QueryParam "chat_id" Text
         :> QueryParam "user_id" Int
         :> Post '[JSON] KickChatMemberResponse
    :<|> TelegramToken :> "leaveChat"
         :> QueryParam "chat_id" Text
         :> Post '[JSON] LeaveChatResponse
    :<|> TelegramToken :> "unbanChatMember"
         :> QueryParam "chat_id" Text
         :> QueryParam "user_id" Int
         :> Post '[JSON] UnbanChatMemberResponse
    :<|> TelegramToken :> "getChat"
         :> QueryParam "chat_id" Text
         :> Post '[JSON] GetChatResponse
    :<|> TelegramToken :> "getChatAdministrators"
         :> QueryParam "chat_id" Text
         :> Post '[JSON] GetChatAdministratorsResponse
    :<|> TelegramToken :> "getChatMembersCount"
         :> QueryParam "chat_id" Text
         :> Post '[JSON] GetChatMembersCountResponse
    :<|> TelegramToken :> "getChatMember"
         :> QueryParam "chat_id" Text
         :> QueryParam "user_id" Int
         :> Post '[JSON] GetChatMemberResponse
    :<|> TelegramToken :> "editMessageText"
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

-- | Proxy for Thelegram Bot API
api :: Proxy TelegramBotAPI
api = Proxy

getMe_                     :: Token -> ClientM GetMeResponse
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
sendLocation_              :: Token -> SendLocationRequest -> ClientM MessageResponse
sendVenue_                 :: Token -> SendVenueRequest-> ClientM MessageResponse
sendContact_               :: Token -> SendContactRequest -> ClientM MessageResponse
sendChatAction_            :: Token -> SendChatActionRequest -> ClientM ChatActionResponse
getUpdates_                :: Token -> Maybe Int -> Maybe Int -> Maybe Int -> ClientM UpdatesResponse
getFile_                   :: Token -> Maybe Text -> ClientM FileResponse
getUserProfilePhotos_      :: Token -> Maybe Int -> Maybe Int -> Maybe Int -> ClientM UserProfilePhotosResponse
setWebhook_                :: Token -> Maybe Text -> ClientM SetWebhookResponse
setWebhookWithCert_        :: Token -> SetWebhookRequest -> ClientM SetWebhookResponse
answerInlineQuery_         :: Token -> AnswerInlineQueryRequest -> ClientM InlineQueryResponse
answerCallbackQuery_       :: Token -> AnswerCallbackQueryRequest -> ClientM CallbackQueryResponse
kickChatMember_            :: Token -> Maybe Text -> Maybe Int -> ClientM KickChatMemberResponse
leaveChat_                 :: Token -> Maybe Text -> ClientM LeaveChatResponse
unbanChatMember_           :: Token -> Maybe Text -> Maybe Int -> ClientM UnbanChatMemberResponse
getChat_                   :: Token -> Maybe Text -> ClientM GetChatResponse
getChatAdministrators_     :: Token -> Maybe Text -> ClientM GetChatAdministratorsResponse
getChatMembersCount_       :: Token -> Maybe Text -> ClientM GetChatMembersCountResponse
getChatMember_             :: Token -> Maybe Text -> Maybe Int -> ClientM GetChatMemberResponse
editMessageText_           :: Token -> EditMessageTextRequest -> ClientM MessageResponse
editMessageCaption_        :: Token -> EditMessageCaptionRequest -> ClientM MessageResponse
editMessageReplyMarkup_    :: Token -> EditMessageReplyMarkupRequest -> ClientM MessageResponse
editMessageText__          :: Token -> EditMessageTextRequest -> ClientM (Response Bool)
editMessageCaption__       :: Token -> EditMessageCaptionRequest -> ClientM (Response Bool)
editMessageReplyMarkup__   :: Token -> EditMessageReplyMarkupRequest -> ClientM (Response Bool)
getMe_
  :<|> sendMessage_
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
  :<|> sendLocation_
  :<|> sendVenue_
  :<|> sendContact_
  :<|> sendChatAction_
  :<|> getUpdates_
  :<|> getFile_
  :<|> getUserProfilePhotos_
  :<|> setWebhook_
  :<|> setWebhookWithCert_
  :<|> answerInlineQuery_
  :<|> answerCallbackQuery_
  :<|> kickChatMember_
  :<|> leaveChat_
  :<|> unbanChatMember_
  :<|> getChat_
  :<|> getChatAdministrators_
  :<|> getChatMembersCount_
  :<|> getChatMember_
  :<|> editMessageText_
  :<|> editMessageCaption_
  :<|> editMessageReplyMarkup_
  :<|> editMessageText__
  :<|> editMessageCaption__
  :<|> editMessageReplyMarkup__ =
      client api

-- | A simple method for testing your bot's auth token. Requires no parameters.
--   Returns basic information about the bot in form of a 'User' object.
getMe :: Token -> Manager -> IO (Either ServantError GetMeResponse)
getMe token manager = runClientM (getMe_ token) (ClientEnv manager telegramBaseUrl)

-- | Use this method to send text messages. On success, the sent 'Message' is returned.
sendMessage :: Token -> SendMessageRequest -> Manager -> IO (Either ServantError MessageResponse)
sendMessage = run telegramBaseUrl sendMessage_

-- | Use this method to forward messages of any kind. On success, the sent 'Message' is returned.
forwardMessage :: Token -> ForwardMessageRequest -> Manager -> IO (Either ServantError MessageResponse)
forwardMessage = run telegramBaseUrl forwardMessage_

-- | Use this method to upload and send photos. On success, the sent 'Message' is returned.
uploadPhoto :: Token -> SendPhotoRequest FileUpload -> Manager -> IO (Either ServantError MessageResponse)
uploadPhoto = run telegramBaseUrl uploadPhoto_

-- | Use this method to send photos that have already been uploaded. On success, the sent 'Message' is returned.
sendPhoto :: Token -> SendPhotoRequest Text -> Manager -> IO (Either ServantError MessageResponse)
sendPhoto = run telegramBaseUrl sendPhoto_

-- | Use this method to upload and send audio files, if you want Telegram clients to display them in the music player. Your audio must be in the .mp3 format. On success, the sent 'Message' is returned. Bots can currently send audio files of up to 50 MB in size, this limit may be changed in the future.
--
--       For backward compatibility, when the fields __title__ and __performer__ are both empty and the mime-type of the file to be sent is not _audio/mpeg_, the file will be sent as a playable voice message. For this to work, the audio must be in an .ogg file encoded with OPUS. This behavior will be phased out in the future. For sending voice messages, use the 'sendVoice' method instead.
uploadAudio :: Token -> SendAudioRequest FileUpload -> Manager -> IO (Either ServantError MessageResponse)
uploadAudio = run telegramBaseUrl uploadAudio_

-- | Use this method to send audio files that are already on the Telegram servers, if you want Telegram clients to display them in the music player. Your audio must be in the .mp3 format. On success, the sent 'Message' is returned. Bots can currently send audio files of up to 50 MB in size, this limit may be changed in the future.
--
--       For backward compatibility, when the fields __title__ and __performer__ are both empty and the mime-type of the file to be sent is not _audio/mpeg_, the file will be sent as a playable voice message. For this to work, the audio must be in an .ogg file encoded with OPUS. This behavior will be phased out in the future. For sending voice messages, use the 'sendVoice' method instead.
sendAudio :: Token -> SendAudioRequest Text -> Manager -> IO (Either ServantError MessageResponse)
sendAudio = run telegramBaseUrl sendAudio_

-- | Use this method to upload and send general files. On success, the sent 'Message' is returned. Bots can currently send files of any type of up to 50 MB in size, this limit may be changed in the future.
uploadDocument :: Token -> SendDocumentRequest FileUpload -> Manager -> IO (Either ServantError MessageResponse)
uploadDocument = run telegramBaseUrl uploadDocument_

-- | Use this method to send general files that have already been uploaded. On success, the sent 'Message' is returned. Bots can currently send files of any type of up to 50 MB in size, this limit may be changed in the future.
sendDocument :: Token -> SendDocumentRequest Text -> Manager -> IO (Either ServantError MessageResponse)
sendDocument = run telegramBaseUrl sendDocument_

-- | Use this method to upload and send .webp stickers. On success, the sent 'Message' is returned.
uploadSticker :: Token -> SendStickerRequest FileUpload -> Manager -> IO (Either ServantError MessageResponse)
uploadSticker = run telegramBaseUrl uploadSticker_

-- | Use this method to send .webp stickers that are already on the Telegram servers. On success, the sent 'Message' is returned.
sendSticker :: Token -> SendStickerRequest Text -> Manager -> IO (Either ServantError MessageResponse)
sendSticker = run telegramBaseUrl sendSticker_

-- | Use this method to upload and send video files. Telegram clients support mp4 videos (other formats may be sent as 'Document'). On success, the sent 'Message' is returned. Bots can currently send video files of up to 50 MB in size, this limit may be changed in the future.
uploadVideo :: Token -> SendVideoRequest FileUpload -> Manager -> IO (Either ServantError MessageResponse)
uploadVideo = run telegramBaseUrl uploadVideo_

-- | Use this method to send video files that are already on the Telegram servers. Telegram clients support mp4 videos (other formats may be sent as 'Document'). On success, the sent 'Message' is returned. Bots can currently send video files of up to 50 MB in size, this limit may be changed in the future.
sendVideo :: Token -> SendVideoRequest Text -> Manager -> IO (Either ServantError MessageResponse)
sendVideo = run telegramBaseUrl sendVideo_

-- | Use this method to upload and send audio files, if you want Telegram clients to display the file as a playable voice message. For this to work, your audio must be in an .ogg file encoded with OPUS (other formats may be sent as 'Audio' or 'Document'). On success, the sent 'Message' is returned. Bots can currently send voice messages of up to 50 MB in size, this limit may be changed in the future.
uploadVoice :: Token -> SendVoiceRequest FileUpload -> Manager -> IO (Either ServantError MessageResponse)
uploadVoice = run telegramBaseUrl uploadVoice_

-- | Use this method to send audio files that are already on the telegram server, if you want Telegram clients to display the file as a playable voice message. For this to work, your audio must be in an .ogg file encoded with OPUS (other formats may be sent as 'Audio' or 'Document'). On success, the sent 'Message' is returned. Bots can currently send voice messages of up to 50 MB in size, this limit may be changed in the future.
sendVoice :: Token -> SendVoiceRequest Text -> Manager -> IO (Either ServantError MessageResponse)
sendVoice = run telegramBaseUrl sendVoice_

-- | Use this method to send point on the map. On success, the sent 'Message' is returned.
sendLocation :: Token -> SendLocationRequest -> Manager -> IO (Either ServantError MessageResponse)
sendLocation = run telegramBaseUrl sendLocation_

-- | Use this method to send information about a venue. On success, the sent 'Message' is returned.
sendVenue :: Token -> SendVenueRequest -> Manager -> IO (Either ServantError MessageResponse)
sendVenue = run telegramBaseUrl sendVenue_

-- | Use this method to send information about a venue. On success, the sent 'Message' is returned.
sendContact :: Token -> SendContactRequest -> Manager -> IO (Either ServantError MessageResponse)
sendContact = run telegramBaseUrl sendContact_

-- | Use this method when you need to tell the user that something is happening on the bot's side.
--   The status is set for 5 seconds or less (when a message arrives from your bot,
--   Telegram clients clear its typing status).
sendChatAction :: Token -> SendChatActionRequest -> Manager -> IO (Either ServantError ChatActionResponse)
sendChatAction = run telegramBaseUrl sendChatAction_

-- | Use this method to receive incoming updates using long polling. An Array of 'Update' objects is returned.
getUpdates
    :: Token
    -> Maybe Int -- ^ offset
    -> Maybe Int -- ^ limit
    -> Maybe Int -- ^ timeout
    -> Manager
    -> IO (Either ServantError UpdatesResponse)
getUpdates token offset limit timeout manager = runClientM (getUpdates_ token offset limit timeout) $ ClientEnv manager telegramBaseUrl

-- | Use this method to get basic info about a file and prepare it for downloading. For the moment, bots can download files of up to 20MB in size. On success, a 'File' object is returned. The file can then be downloaded via the link @https://api.telegram.org/file/bot<token>/<file_path>@, where @<file_path>@ is taken from the response. It is guaranteed that the link will be valid for at least 1 hour. When the link expires, a new one can be requested by calling getFile again.
getFile :: Token -> Text -> Manager -> IO (Either ServantError FileResponse)
getFile token file_id manager = runClientM (getFile_ token (Just file_id)) $ ClientEnv manager telegramBaseUrl

-- | Use this method to get a list of profile pictures for a user. Returns a 'UserProfilePhotos' object.
getUserProfilePhotos :: Token -> Int -> Maybe Int -> Maybe Int -> Manager -> IO (Either ServantError UserProfilePhotosResponse)
getUserProfilePhotos token user_id offset limit manager = runClientM (getUserProfilePhotos_ token (Just user_id) offset limit) $ ClientEnv manager telegramBaseUrl

-- | Use this method to specify a url and receive incoming updates via an outgoing webhook. Whenever there is an update for the bot, we will send an HTTPS POST request to the specified url, containing a JSON-serialized 'Update'. In case of an unsuccessful request, we will give up after a reasonable amount of attempts.
--
--       If you'd like to make sure that the Webhook request comes from Telegram, we recommend using a secret path in the URL, e.g. @https://www.example.com/<token>@. Since nobody else knows your bot‘s token, you can be pretty sure it’s us.
setWebhook :: Token
    -> Maybe Text -- ^ HTTPS url to send updates to. Use an empty string to remove webhook integration
    -> Manager
    -> IO (Either ServantError SetWebhookResponse)
setWebhook token url manager = runClientM (setWebhook_ token url) $ ClientEnv manager telegramBaseUrl

-- | Use this method to specify a url and receive incoming updates via an outgoing webhook. Whenever there is an update for the bot, we will send an HTTPS POST request to the specified url, containing a JSON-serialized 'Update'. In case of an unsuccessful request, we will give up after a reasonable amount of attempts.
--
--       If you'd like to make sure that the Webhook request comes from Telegram, we recommend using a secret path in the URL, e.g. @https://www.example.com/<token>@. Since nobody else knows your bot‘s token, you can be pretty sure it’s us.
setWebhookWithCertificate :: Token -> SetWebhookRequest -> Manager -> IO (Either ServantError SetWebhookResponse)
setWebhookWithCertificate = run telegramBaseUrl setWebhookWithCert_

-- | Use this method to send answers to an inline query. No more than 50 results per query are allowed.
answerInlineQuery :: Token -> AnswerInlineQueryRequest -> Manager -> IO (Either ServantError InlineQueryResponse)
answerInlineQuery = run telegramBaseUrl answerInlineQuery_

-- | Use this method to send answers to callback queries sent from inline keyboards. The answer will be displayed to the user as a notification at the top of the chat screen or as an alert.
answerCallbackQuery :: Token -> AnswerCallbackQueryRequest -> Manager -> IO (Either ServantError CallbackQueryResponse)
answerCallbackQuery = run telegramBaseUrl answerCallbackQuery_

-- | Use this method to kick a user from a group or a supergroup. In the case of supergroups, the user will not be able to return to the group on their own using invite links, etc., unless unbanned first. The bot must be an administrator in the group for this to work.
kickChatMember :: Token -> Text -> Int -> Manager -> IO (Either ServantError KickChatMemberResponse)
kickChatMember token chat_id user_id manager = runClientM (kickChatMember_ token (Just chat_id) (Just user_id)) $ ClientEnv manager telegramBaseUrl

-- | Use this method for your bot to leave a group, supergroup or channel. Returns True on success.
leaveChat :: Token -> Text -> Manager -> IO (Either ServantError LeaveChatResponse)
leaveChat token chat_id manager = runClientM (leaveChat_ token (Just chat_id)) $ ClientEnv manager telegramBaseUrl

-- | Use this method to unban a previously kicked user in a supergroup. The user will not return to the group automatically, but will be able to join via link, etc. The bot must be an administrator in the group for this to work.
unbanChatMember :: Token -> Text -> Int -> Manager -> IO (Either ServantError UnbanChatMemberResponse)
unbanChatMember token chat_id user_id manager = runClientM (unbanChatMember_ token (Just chat_id) (Just user_id)) $ ClientEnv manager telegramBaseUrl

-- | Use this method to get up to date information about the chat (current name of the user for one-on-one conversations, current username of a user, group or channel, etc.)
getChat :: Token -> Text -> Manager -> IO (Either ServantError GetChatResponse)
getChat token chat_id manager = runClientM (getChat_ token (Just chat_id)) $ ClientEnv manager telegramBaseUrl

-- | Use this method to get a list of administrators in a chat. On success, returns an Array of 'ChatMember' objects that contains information about all chat administrators except other bots. If the chat is a group or a supergroup and no administrators were appointed, only the creator will be returned.
getChatAdministrators :: Token -> Text -> Manager -> IO (Either ServantError GetChatAdministratorsResponse)
getChatAdministrators token chat_id manager = runClientM (getChatAdministrators_ token (Just chat_id)) $ ClientEnv manager telegramBaseUrl

-- | Use this method to get the number of members in a chat. Returns 'Int' on success.
getChatMembersCount :: Token -> Text -> Manager -> IO (Either ServantError GetChatMembersCountResponse)
getChatMembersCount token chat_id manager = runClientM (getChatMembersCount_ token (Just chat_id)) $ ClientEnv manager telegramBaseUrl

-- | Use this method to get information about a member of a chat. Returns a 'ChatMember' object on success.
getChatMember :: Token -> Text -> Int -> Manager -> IO (Either ServantError GetChatMemberResponse)
getChatMember token chat_id user_id manager = runClientM (getChatMember_ token (Just chat_id) (Just user_id)) $ ClientEnv manager telegramBaseUrl

-- | Use this method to edit text messages sent by the bot. On success, the edited 'Message' is returned, otherwise True is returned.
editMessageText :: Token -> EditMessageTextRequest -> Manager -> IO (Either ServantError MessageResponse)
editMessageText = run telegramBaseUrl editMessageText_

-- | Use this method to edit captions of messages sent by the bot. On success, the edited 'Message' is returned.
editMessageCaption :: Token -> EditMessageCaptionRequest -> Manager -> IO (Either ServantError MessageResponse)
editMessageCaption = run telegramBaseUrl editMessageCaption_

-- | Use this method to edit only the reply markup of messages sent by the bot. On success, the edited 'Message' is returned.
editMessageReplyMarkup :: Token -> EditMessageReplyMarkupRequest -> Manager -> IO (Either ServantError MessageResponse)
editMessageReplyMarkup = run telegramBaseUrl editMessageReplyMarkup_

-- | Use this method to edit text messages sent via the bot (for inline bots).
editInlineMessageText :: Token -> EditMessageTextRequest -> Manager -> IO (Either ServantError (Response Bool))
editInlineMessageText = run telegramBaseUrl editMessageText__

-- | Use this method to edit captions of messages sent via the bot (for inline bots).
editInlineMessageCaption :: Token -> EditMessageCaptionRequest -> Manager -> IO (Either ServantError (Response Bool))
editInlineMessageCaption = run telegramBaseUrl editMessageCaption__

-- | Use this method to edit only the reply markup of messages sent via the bot (for inline bots).
editInlineMessageReplyMarkup :: Token -> EditMessageReplyMarkupRequest -> Manager -> IO (Either ServantError (Response Bool))
editInlineMessageReplyMarkup = run telegramBaseUrl editMessageReplyMarkup__

run :: BaseUrl -> (Token -> a -> ClientM b) -> Token -> a -> Manager -> IO (Either ServantError b)
run b e t r m = runClientM (e t r) (ClientEnv m b)
