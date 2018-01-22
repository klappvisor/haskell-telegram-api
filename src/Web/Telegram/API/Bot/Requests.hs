{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains data objects which represents requests to Telegram Bot API
module Web.Telegram.API.Bot.Requests
    ( -- * Types
      ChatId                         (..)
    , SendMessageRequest             (..)
    , ForwardMessageRequest          (..)
    , FileUpload                     (..)
    , FileUploadContent              (..)
    , SetWebhookRequest              (..)
    , GetUpdatesRequest              (..)
    , SendPhotoRequest               (..)
    , SendAudioRequest               (..)
    , SendDocumentRequest            (..)
    , SendStickerRequest             (..)
    , SendVideoRequest               (..)
    , SendVoiceRequest               (..)
    , SendVideoNoteRequest           (..)
    , SendMediaGroupRequest          (..)
    , SendLocationRequest            (..)
    , SendVenueRequest               (..)
    , SendContactRequest             (..)
    , SendChatActionRequest          (..)
    , SendGameRequest                (..)
    , ChatAction                     (..)
    , AnswerInlineQueryRequest       (..)
    , AnswerCallbackQueryRequest     (..)
    , ReplyKeyboard                  (..)
    , EditMessageTextRequest         (..)
    , EditMessageCaptionRequest      (..)
    , EditMessageReplyMarkupRequest  (..)
    , SendInvoiceRequest             (..)
    , AnswerShippingQueryRequest     (..)
    , AnswerPreCheckoutQueryRequest  (..)
    , RestrictChatMemberRequest      (..)
    , PromoteChatMemberRequest       (..)
    , SetChatPhotoRequest            (..)
    , UploadStickerFileRequest       (..)
    , CreateNewStickerSetRequest     (..)
    , AddStickerToSetRequest         (..)
    , EditMessageLiveLocationRequest (..)
    , StopMessageLiveLocationRequest (..)
     -- * Functions
    , localFileUpload
    , setWebhookRequest
    , setWebhookRequest'
    , getUpdatesRequest
    , sendMessageRequest
    , forwardMessageRequest
    , sendPhotoRequest
    , uploadPhotoRequest
    , sendAudioRequest
    , uploadAudioRequest
    , sendDocumentRequest
    , uploadDocumentRequest
    , sendStickerRequest
    , uploadStickerRequest
    , sendVideoRequest
    , uploadVideoRequest
    , sendVoiceRequest
    , uploadVoiceRequest
    , sendVideoNoteRequest
    , uploadVideoNoteRequest
    , sendMediaGroupRequest
    , sendLocationRequest
    , sendVenueRequest
    , sendContactRequest
    , sendChatActionRequest
    , sendGameRequest
    , answerInlineQueryRequest
    , answerCallbackQueryRequest
    , inlineKeyboardMarkup
    , replyKeyboardMarkup
    , replyKeyboardHide
    , forceReply
    , editMessageTextRequest
    , editInlineMessageTextRequest
    , editMessageCaptionRequest
    , editInlineMessageCaptionRequest
    , editMessageReplyMarkupRequest
    , editInlineMessageReplyMarkupRequest
    , sendInvoiceRequest
    , okShippingQueryRequest
    , errorShippingQueryRequest
    , okAnswerPrecheckoutQueryRequest
    , errorAnswerPrecheckoutQueryRequest
    , restrictChatMemberRequest
    , promoteChatMemberRequest
    ) where

import           Data.Aeson
import           Data.Aeson.Types                      (typeMismatch)
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Lazy                  as LBS
import           Data.Int                              (Int64)
import           Data.Maybe
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as T
import           GHC.Generics
import           Network.HTTP.Client.MultipartFormData
import           Network.Mime
import           Servant.Client.MultipartFormData      (ToMultipartFormData (..))
import           Web.Telegram.API.Bot.Data             (CurrencyCode,
                                                        InlineKeyboardButton,
                                                        InlineKeyboardMarkup,
                                                        InlineQueryResult,
                                                        InputMedia,
                                                        KeyboardButton,
                                                        LabeledPrice,
                                                        MaskPosition, ParseMode,
                                                        ShippingOption)
import           Web.Telegram.API.Bot.JsonExt


-- | This object represents data (image, video, ...) to upload.
data FileUploadContent =
    FileUploadFile FilePath
  | FileUploadBS BS.ByteString
  | FileUploadLBS LBS.ByteString

-- | This object represents data (image, video, ...) with mime type to upload.
data FileUpload = FileUpload
  {
    fileUpload_type    :: Maybe MimeType    -- ^ Mime type of the upload.
  , fileUpload_content :: FileUploadContent -- ^ The payload/source to upload.
  }

localFileUpload :: FilePath -> FileUpload
localFileUpload path = FileUpload
  { fileUpload_type = Nothing
  , fileUpload_content = FileUploadFile path
  }

fileUploadToPart :: Text -> FileUpload -> Part
fileUploadToPart inputName fileUpload =
  let part =
        case fileUpload_content fileUpload of
          FileUploadFile path -> partFileSource inputName path
          FileUploadBS bs     -> partBS inputName bs
          FileUploadLBS lbs   -> partLBS inputName lbs
  in part { partContentType = fileUpload_type fileUpload }

utf8Part :: Text -> Text -> Part
utf8Part inputName = partBS inputName . T.encodeUtf8

-- | This object represents request for 'setWebhookWithCertificate'
data SetWebhookRequest = SetWebhookRequest
  {
    webhook_url             :: Text -- ^ HTTPS url to send updates to. Use `setWebhook` function and an empty string to remove webhook integration
  , webhook_certificate     :: FileUpload -- ^ Upload your public key certificate so that the root certificate in use can be checked.
  , webhook_max_connections :: Maybe Int
-- TODO: implement , webhook_allowed_updates :: Maybe [Text]
  }
  | SetWebhookWithoutCertRequest
  {
    webhook_url             :: Text
  , webhook_max_connections :: Maybe Int
  , webhook_allowed_updates :: Maybe [Text]
  } deriving (Generic)

instance ToJSON SetWebhookRequest where
  toJSON SetWebhookRequest{} = undefined
  toJSON (SetWebhookWithoutCertRequest url maxConnections allowedUpdates) = object $
    ("url" .= url) : catMaybes [
    ("max_connections" .=) <$> maxConnections,
    ("allowed_updates" .=) <$> allowedUpdates]

instance ToMultipartFormData SetWebhookRequest where
  toMultipartFormData req =
    [ utf8Part         "url"         $ webhook_url req
    , fileUploadToPart "certificate" $ webhook_certificate req ] ++
    catMaybes
    [ utf8Part         "max_connections" . tshow <$> webhook_max_connections req
    -- TODO: , ??? "allowed_updates" ??? <$> webhook_allowed_updates req
    ]

setWebhookRequest :: Text -> FileUpload -> SetWebhookRequest
setWebhookRequest url certificate = SetWebhookRequest url certificate Nothing

setWebhookRequest' :: Text -> SetWebhookRequest
setWebhookRequest' url = SetWebhookWithoutCertRequest url Nothing Nothing

data GetUpdatesRequest = GetUpdatesRequest
  {
    updates_offset          :: Maybe Int -- ^ Identifier of the first update to be returned. Must be greater by one than the highest among the identifiers of previously received updates. By default, updates starting with the earliest unconfirmed update are returned. An update is considered confirmed as soon as 'getUpdates' is called with an offset higher than its update_id. The negative offset can be specified to retrieve updates starting from -offset update from the end of the updates queue. All previous updates will forgotten.
  , updates_limit           :: Maybe Int -- ^ Limits the number of updates to be retrieved. Values between 1—100 are accepted. Defaults to 100.
  , updates_timeout         :: Maybe Int -- ^ Timeout in seconds for long polling. Defaults to 0, i.e. usual short polling. Should be positive, short polling should be used for testing purposes only.
  , updates_allowed_updates :: Maybe [Text] -- ^ List the types of updates you want your bot to receive. For example, specify [“message”, “edited_channel_post”, “callback_query”] to only receive updates of these types. See 'Update' for a complete list of available update types. Specify an empty list to receive all updates regardless of type (default). If not specified, the previous setting will be used.
  } deriving (Show, Generic)

instance ToJSON GetUpdatesRequest where
  toJSON = toJsonDrop 8

instance FromJSON GetUpdatesRequest where
  parseJSON = parseJsonDrop 8

getUpdatesRequest :: GetUpdatesRequest
getUpdatesRequest = GetUpdatesRequest Nothing Nothing Nothing Nothing

-- | Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
data ChatId = ChatId Int64 | ChatChannel Text
  deriving (Show)

instance ToJSON ChatId where
  toJSON (ChatId integer)   = toJSON integer
  toJSON (ChatChannel text) = String text

instance FromJSON ChatId where
  parseJSON value@Number{} = ChatId <$> parseJSON value
  parseJSON (String text)  = pure $ ChatChannel text
  parseJSON wat            = typeMismatch "Int64 or String" wat

chatIdToPart :: ChatId -> Text
chatIdToPart (ChatId integer)   = tshow integer
chatIdToPart (ChatChannel text) = text

-- | This object represents request for 'sendMessage'
data SendMessageRequest = SendMessageRequest
  {
    message_chat_id                  :: ChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , message_text                     :: Text -- ^ Text of the message to be sent
  , message_parse_mode               :: Maybe ParseMode -- ^ Send 'Markdown', if you want Telegram apps to show bold, italic and inline URLs in your bot's message
  , message_disable_web_page_preview :: Maybe Bool -- ^ Disables link previews for links in this message
  , message_disable_notification     :: Maybe Bool -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , message_reply_to_message_id      :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , message_reply_markup             :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Show, Generic)

instance ToJSON SendMessageRequest where
  toJSON = toJsonDrop 8

instance FromJSON SendMessageRequest where
  parseJSON = parseJsonDrop 8

sendMessageRequest :: ChatId -> Text -> SendMessageRequest
sendMessageRequest chatId text = SendMessageRequest chatId text Nothing Nothing Nothing Nothing Nothing

-- | This object represents request for 'forwardMessage'
data ForwardMessageRequest = ForwardMessageRequest
  {
    forward_chat_id              :: ChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , forward_from_chat_id         :: ChatId -- ^ Unique identifier for the chat where the original message was sent (or channel username in the format @@channelusername@)
  , forward_disable_notification :: Maybe Bool -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , forward_message_id           :: Int -- ^ Unique message identifier
  } deriving (Show, Generic)

instance ToJSON ForwardMessageRequest where
  toJSON = toJsonDrop 8

instance FromJSON ForwardMessageRequest where
  parseJSON = parseJsonDrop 8

forwardMessageRequest :: ChatId -> ChatId -> Int -> ForwardMessageRequest
forwardMessageRequest chatId fromChatId forwardMessageId = ForwardMessageRequest chatId fromChatId Nothing forwardMessageId

-- | This object represents request for 'sendPhoto'
data SendPhotoRequest payload = SendPhotoRequest
  {
    photo_chat_id              :: ChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , photo_photo                :: payload -- ^ Photo to send. You can either pass a file_id as String to resend a photo that is already on the Telegram servers, or upload a new photo.
  , photo_caption              :: Maybe Text -- ^ Photo caption (may also be used when resending photos by file_id), 0-200 characters.
  , photo_disable_notification :: Maybe Bool -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , photo_reply_to_message_id  :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , photo_reply_markup         :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Show, Generic)

instance ToJSON (SendPhotoRequest Text) where
  toJSON = toJsonDrop 6

instance FromJSON (SendPhotoRequest Text) where
  parseJSON = parseJsonDrop 6

sendPhotoRequest :: ChatId -> Text -> SendPhotoRequest Text
sendPhotoRequest chatId photo = SendPhotoRequest chatId photo Nothing Nothing Nothing Nothing

uploadPhotoRequest :: ChatId -> FileUpload -> SendPhotoRequest FileUpload
uploadPhotoRequest chatId photo = SendPhotoRequest chatId photo Nothing Nothing Nothing Nothing

instance ToMultipartFormData (SendPhotoRequest FileUpload) where
  toMultipartFormData req =
    [ utf8Part "chat_id" (chatIdToPart $ photo_chat_id req) ] ++
    catMaybes
    [ utf8Part "caption" <$> photo_caption req
    , partLBS "disable_notification" . encode <$> photo_disable_notification req
    , utf8Part "reply_to_message_id" . tshow <$> photo_reply_to_message_id req
    , partLBS "reply_markup" . encode <$> photo_reply_markup req
    ] ++
    [ fileUploadToPart "photo" (photo_photo req) ]

-- | This object represents request for 'sendAudio'
data SendAudioRequest payload = SendAudioRequest
  {
    _audio_chat_id              :: ChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , _audio_audio                :: payload -- ^ Audio to send. You can either pass a file_id as String to resend an audio that is already on the Telegram servers, or upload a new audio file.
  , _audio_caption              :: Maybe Text -- ^ Audio caption, 0-200 characters
  , _audio_duration             :: Maybe Int -- ^ Duration of the audio in seconds
  , _audio_performer            :: Maybe Text -- ^ Performer
  , _audio_title                :: Maybe Text -- ^ Track name
  , _audio_disable_notification :: Maybe Bool -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , _audio_reply_to_message_id  :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , _audio_reply_markup         :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Show, Generic)

instance ToJSON (SendAudioRequest Text) where
  toJSON = toJsonDrop 7

instance FromJSON (SendAudioRequest Text) where
  parseJSON = parseJsonDrop 7

instance ToMultipartFormData (SendAudioRequest FileUpload) where
  toMultipartFormData req =
    [ utf8Part "chat_id" (chatIdToPart $ _audio_chat_id req) ] ++
    catMaybes
    [ utf8Part "duration" . tshow <$> _audio_duration req
    , utf8Part "performer" <$> _audio_performer req
    , utf8Part "title" <$> _audio_title req
    , partLBS "disable_notification" . encode <$> _audio_disable_notification req
    , utf8Part "reply_to_message_id" . tshow <$> _audio_reply_to_message_id req
    , partLBS "reply_markup" . encode <$> _audio_reply_markup req
    ] ++
    [ fileUploadToPart "audio" (_audio_audio req) ]

sendAudioRequest :: ChatId -> Text -> SendAudioRequest Text
sendAudioRequest chatId audio = SendAudioRequest chatId audio Nothing Nothing Nothing Nothing Nothing Nothing Nothing

uploadAudioRequest :: ChatId -> FileUpload -> SendAudioRequest FileUpload
uploadAudioRequest chatId audio = SendAudioRequest chatId audio Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | This object represents request for 'sendSticker'
data SendStickerRequest payload = SendStickerRequest
  {
    sticker_chat_id              :: ChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , sticker_sticker              :: payload -- ^ Sticker to send. You can either pass a file_id as String to resend a sticker that is already on the Telegram servers, or upload a new sticker.
  , sticker_disable_notification :: Maybe Bool -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , sticker_reply_to_message_id  :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , sticker_reply_markup         :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Show, Generic)

instance ToJSON (SendStickerRequest Text) where
  toJSON = toJsonDrop 8

instance FromJSON (SendStickerRequest Text) where
  parseJSON = parseJsonDrop 8

instance ToMultipartFormData (SendStickerRequest FileUpload) where
  toMultipartFormData req =
    [ utf8Part "chat_id" (chatIdToPart $ sticker_chat_id req) ] ++
    catMaybes
    [ partLBS "disable_notification" . encode <$> sticker_disable_notification req
    , utf8Part "reply_to_message_id" . tshow <$> sticker_reply_to_message_id req
    , partLBS "reply_markup" . encode <$> sticker_reply_markup req
    ] ++
    [ fileUploadToPart "sticker" (sticker_sticker req) ]

sendStickerRequest :: ChatId -> Text -> SendStickerRequest Text
sendStickerRequest chatId sticker = SendStickerRequest chatId sticker Nothing Nothing Nothing

uploadStickerRequest :: ChatId -> FileUpload -> SendStickerRequest FileUpload
uploadStickerRequest chatId sticker = SendStickerRequest chatId sticker Nothing Nothing Nothing

-- | This object represents request for 'sendDocument'
data SendDocumentRequest payload = SendDocumentRequest
  {
    document_chat_id              :: ChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , document_document             :: payload -- ^ File to send. You can either pass a file_id as String to resend a file that is already on the Telegram servers, or upload a new file.
  , document_caption              :: Maybe Text -- ^ Document caption (may also be used when resending documents by file_id), 0-200 characters
  , document_disable_notification :: Maybe Bool -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , document_reply_to_message_id  :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , document_reply_markup         :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Show, Generic)

instance ToJSON (SendDocumentRequest Text) where
  toJSON = toJsonDrop 9

instance FromJSON (SendDocumentRequest Text) where
  parseJSON = parseJsonDrop 9

instance ToMultipartFormData (SendDocumentRequest FileUpload) where
  toMultipartFormData req =
    [ utf8Part "chat_id" (chatIdToPart $ document_chat_id req) ] ++
    catMaybes
    [ utf8Part "caption" <$> document_caption req
    , partLBS "disable_notification" . encode <$> document_disable_notification req
    , utf8Part "reply_to_message_id" . tshow <$> document_reply_to_message_id req
    , partLBS "reply_markup" . encode <$> document_reply_markup req
    ] ++
    [ fileUploadToPart "document" (document_document req) ]

sendDocumentRequest :: ChatId -> Text -> SendDocumentRequest Text
sendDocumentRequest chatId document = SendDocumentRequest chatId document Nothing Nothing Nothing Nothing

uploadDocumentRequest :: ChatId -> FileUpload -> SendDocumentRequest FileUpload
uploadDocumentRequest chatId document = SendDocumentRequest chatId document Nothing Nothing Nothing Nothing

-- | This object represents request for 'sendVideo'
data SendVideoRequest payload = SendVideoRequest
  {
    _video_chat_id              :: ChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , _video_video                :: payload -- ^ Video to send. You can either pass a file_id as String to resend a video that is already on the Telegram servers, or upload a new video.
  , _video_duration             :: Maybe Int -- ^ Duration of sent video in seconds
  , _video_caption              :: Maybe Text -- ^ Video caption, 0-200 characters.
  , _video_disable_notification :: Maybe Bool -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , _video_reply_to_message_id  :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , _video_reply_markup         :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Show, Generic)

instance ToJSON (SendVideoRequest Text) where
  toJSON = toJsonDrop 7

instance FromJSON (SendVideoRequest Text) where
  parseJSON = parseJsonDrop 7

instance ToMultipartFormData (SendVideoRequest FileUpload) where
  toMultipartFormData req =
    [ utf8Part "chat_id" (chatIdToPart $ _video_chat_id req) ] ++
    catMaybes
    [ partLBS "duration" . encode <$> _video_duration req
    , utf8Part "caption" <$> _video_caption req
    , partLBS "disable_notification" . encode <$> _video_disable_notification req
    , utf8Part "reply_to_message_id" . tshow <$> _video_reply_to_message_id req
    , partLBS "reply_markup" . encode <$> _video_reply_markup req
    ] ++
    [ fileUploadToPart "video" (_video_video req) ]

sendVideoRequest :: ChatId -> Text -> SendVideoRequest Text
sendVideoRequest chatId video = SendVideoRequest chatId video Nothing Nothing Nothing Nothing Nothing

uploadVideoRequest :: ChatId -> FileUpload -> SendVideoRequest FileUpload
uploadVideoRequest chatId video = SendVideoRequest chatId video Nothing Nothing Nothing Nothing Nothing

-- | This object represents request for 'sendVoice'
data SendVoiceRequest payload = SendVoiceRequest
  {
    _voice_chat_id              :: ChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , _voice_voice                :: payload -- ^ Audio file to send. You can either pass a file_id as String to resend an audio that is already on the Telegram servers, or upload a new audio file.
  , _voice_caption              :: Maybe Text -- ^ Voice message caption, 0-200 characters
  , _voice_duration             :: Maybe Int -- ^ Duration of sent audio in seconds
  , _voice_disable_notification :: Maybe Bool -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , _voice_reply_to_message_id  :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , _voice_reply_markup         :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Show, Generic)

instance ToJSON (SendVoiceRequest Text) where
  toJSON = toJsonDrop 7

instance FromJSON (SendVoiceRequest Text) where
  parseJSON = parseJsonDrop 7

instance ToMultipartFormData (SendVoiceRequest FileUpload) where
  toMultipartFormData req =
    [ utf8Part "chat_id" (chatIdToPart $ _voice_chat_id req) ] ++
    catMaybes
    [ partLBS "duration" . encode <$> _voice_duration req
    , partLBS "disable_notification" . encode <$> _voice_disable_notification req
    , utf8Part "reply_to_message_id" . tshow <$> _voice_reply_to_message_id req
    , partLBS "reply_markup" . encode <$> _voice_reply_markup req
    ] ++
    [ fileUploadToPart "voice" (_voice_voice req) ]

sendVoiceRequest :: ChatId -> Text -> SendVoiceRequest Text
sendVoiceRequest chatId voice = SendVoiceRequest chatId voice Nothing Nothing Nothing Nothing Nothing

uploadVoiceRequest :: ChatId -> FileUpload -> SendVoiceRequest FileUpload
uploadVoiceRequest chatId voice = SendVoiceRequest chatId voice Nothing Nothing Nothing Nothing Nothing

data SendVideoNoteRequest payload = SendVideoNoteRequest
  {
    _vid_note_chat_id              :: ChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , _vid_note_video_note           :: payload -- ^ Video note to send. Pass a file_id as String to send a video note that exists on the Telegram servers (recommended) or upload a new video using multipart/form-data. More info on Sending Files ». Sending video notes by a URL is currently unsupported
  , _vid_note_duration             :: Maybe Int -- ^ Duration of sent video in seconds
  , _vid_note_length               :: Maybe Int -- ^ Video width and height
  , _vid_note_disable_notification :: Maybe Bool -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , _vid_note_reply_to_message_id  :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , _vid_note_reply_markup         :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  } deriving (Show, Generic)

instance ToJSON (SendVideoNoteRequest Text) where
  toJSON = toJsonDrop 9

instance FromJSON (SendVideoNoteRequest Text) where
  parseJSON = parseJsonDrop 9

instance ToMultipartFormData (SendVideoNoteRequest FileUpload) where
  toMultipartFormData req =
    [ utf8Part "chat_id" (chatIdToPart $ _vid_note_chat_id req) ] ++
    catMaybes
    [ partLBS "duration" . encode <$> _vid_note_duration req
    , partLBS "length" . encode <$> _vid_note_length req
    , partLBS "disable_notification" . encode <$> _vid_note_disable_notification req
    , utf8Part "reply_to_message_id" . tshow <$> _vid_note_reply_to_message_id req
    , partLBS "reply_markup" . encode <$> _vid_note_reply_markup req
    ] ++
    [ fileUploadToPart "video_note" (_vid_note_video_note req) ]

sendVideoNoteRequest :: ChatId -> Text -> SendVideoNoteRequest Text
sendVideoNoteRequest chatId videoNote = SendVideoNoteRequest chatId videoNote Nothing Nothing Nothing Nothing Nothing

uploadVideoNoteRequest :: ChatId -> FileUpload -> SendVideoNoteRequest FileUpload
uploadVideoNoteRequest chatId videoNote = SendVideoNoteRequest chatId videoNote Nothing Nothing Nothing Nothing Nothing

-- | This object represents request for 'sendLocation'
data SendLocationRequest = SendLocationRequest
  {
    location_chat_id              :: ChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , location_latitude             :: Float -- ^ Latitude of location
  , location_longitude            :: Float -- ^ Longitude of location
  , location_live_period          :: Maybe Int -- ^ Period in seconds for which the location will be updated, should be between 60 and 86400.
  , location_disable_notification :: Maybe Bool -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , location_reply_to_message_id  :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , location_reply_markup         :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Show, Generic)

instance ToJSON SendLocationRequest where
  toJSON = toJsonDrop 9

instance FromJSON SendLocationRequest where
  parseJSON = parseJsonDrop 9

sendLocationRequest :: ChatId -> Float -> Float -> SendLocationRequest
sendLocationRequest chatId latitude longitude = SendLocationRequest chatId latitude longitude Nothing Nothing Nothing Nothing

-- | This object represents request for 'sendMediaGroup'
data SendMediaGroupRequest = SendMediaGroupRequest
  {
    media_group_chat_id              :: ChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , media_group_media                :: [InputMedia] -- ^ Array describing photos and videos to be sent, must include 2–10 items
  , media_group_disable_notification :: Maybe Bool -- ^ Sends the messages silently. Users will receive a notification with no sound.
  , media_group_reply_to_message_id  :: Maybe Int -- ^ If the messages are a reply, ID of the original message
  } deriving(Show, Generic)

instance ToJSON SendMediaGroupRequest where
  toJSON = toJsonDrop 12

instance FromJSON SendMediaGroupRequest where
  parseJSON = parseJsonDrop 12

sendMediaGroupRequest :: ChatId -> [InputMedia] -> SendMediaGroupRequest
sendMediaGroupRequest chatId inputMediaArray = SendMediaGroupRequest chatId inputMediaArray Nothing Nothing

-- | This object represents request for 'sendVenue'
data SendVenueRequest = SendVenueRequest
  {
    _venue_chat_id              :: ChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , _venue_latitude             :: Float -- ^ Latitude of the venue
  , _venue_longitude            :: Float -- ^ Longitude of the venue
  , _venue_title                :: Text -- ^ Name of the venue
  , _venue_address              :: Text -- ^ Address of the venue
  , _venue_foursquare_id        :: Maybe Text -- ^ Foursquare identifier of the venue
  , _venue_disable_notification :: Maybe Bool -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , _venue_reply_to_message_id  :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , _venue_reply_markup         :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Show, Generic)

instance ToJSON SendVenueRequest where
  toJSON = toJsonDrop 7

instance FromJSON SendVenueRequest where
  parseJSON = parseJsonDrop 7

sendVenueRequest :: ChatId -> Float -> Float -> Text -> Text -> SendVenueRequest
sendVenueRequest chatId latitude longitude title address = SendVenueRequest chatId latitude longitude title address Nothing Nothing Nothing Nothing

-- | This object represents request for 'sendContact'
data SendContactRequest = SendContactRequest
  {
    _contact_chat_id              :: ChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , _contact_phone_number         :: Text       -- ^ Contact's phone number
  , _contact_first_name           :: Text       -- ^ Contact's first name
  , _contact_last_name            :: Maybe Text -- ^ Contact's last name
  , _contact_disable_notification :: Maybe Bool -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , _contact_reply_to_message_id  :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , _contact_reply_markup         :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Show, Generic)

instance ToJSON SendContactRequest where
  toJSON = toJsonDrop 9

instance FromJSON SendContactRequest where
  parseJSON = parseJsonDrop 9

sendContactRequest :: ChatId -> Text -> Text -> SendContactRequest
sendContactRequest chatId phoneNumber firstName = SendContactRequest chatId phoneNumber firstName Nothing Nothing Nothing Nothing

-- | Type of action to broadcast.
data ChatAction = Typing
                | UploadPhoto
                | RecordVideo
                | UploadVideo
                | RecordAudio
                | UploadAudio
                | UploadDocument
                | FindLocation
                | RecordVideoNote
                | UploadVideoNote deriving (Show, Generic)

instance ToJSON ChatAction where
  toJSON Typing          = "typing"
  toJSON UploadPhoto     = "upload_photo"
  toJSON RecordVideo     = "record_video"
  toJSON UploadVideo     = "upload_video"
  toJSON RecordAudio     = "record_audio"
  toJSON UploadAudio     = "upload_audio"
  toJSON UploadDocument  = "upload_document"
  toJSON FindLocation    = "find_location"
  toJSON RecordVideoNote = "record_video_note"
  toJSON UploadVideoNote = "upload_video_note"

instance FromJSON ChatAction where
  parseJSON "typing"            = pure Typing
  parseJSON "upload_photo"      = pure UploadPhoto
  parseJSON "record_video"      = pure RecordVideo
  parseJSON "upload_video"      = pure UploadVideo
  parseJSON "record_audio"      = pure RecordAudio
  parseJSON "upload_audio"      = pure UploadAudio
  parseJSON "upload_document"   = pure UploadDocument
  parseJSON "find_location"     = pure FindLocation
  parseJSON "record_video_note" = pure RecordVideoNote
  parseJSON "upload_video_note" = pure UploadVideoNote
  parseJSON _                   = fail "Failed to parse ChatAction"

-- | This object represents request for 'sendChatAction'
data SendChatActionRequest = SendChatActionRequest
  {
    action_chat_id :: ChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format `@channelusername`)
  , action_action  :: ChatAction -- ^ Type of action to broadcast.
  } deriving (Show, Generic)

instance ToJSON SendChatActionRequest where
  toJSON = toJsonDrop 7

instance FromJSON SendChatActionRequest where
  parseJSON = parseJsonDrop 7

sendChatActionRequest :: ChatId -> ChatAction -> SendChatActionRequest
sendChatActionRequest = SendChatActionRequest

-- | This object represents request for 'sendGame'
data SendGameRequest = SendGameRequest
  {
    game_chat_id              :: Integer -- ^ Unique identifier for the target chat
  , game_game_short_name      :: Text -- ^ Short name of the game, serves as the unique identifier for the game. Set up your games via Botfather.
  , game_disable_notification :: Maybe Bool -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , game_reply_to_message_id  :: Maybe Int -- ^  If the message is a reply, ID of the original message
  , game_reply_markup         :: Maybe InlineKeyboardMarkup -- ^ A JSON-serialized object for an inline keyboard. If empty, one ‘Play game_title’ button will be shown. If not empty, the first button must launch the game.
  } deriving (Show, Generic)

instance ToJSON SendGameRequest where
  toJSON = toJsonDrop 5

instance FromJSON SendGameRequest where
  parseJSON = parseJsonDrop 5

sendGameRequest :: Integer -> Text -> SendGameRequest
sendGameRequest chatId shortName = SendGameRequest chatId shortName Nothing Nothing Nothing

data AnswerInlineQueryRequest = AnswerInlineQueryRequest
  {
    query_inline_query_id     :: Text -- ^ Unique identifier for the answered query
  , query_results             :: [InlineQueryResult] -- ^ A JSON-serialized array of results for the inline query
  , query_cache_time          :: Maybe Int -- ^ The maximum amount of time in seconds that the result of the inline query may be cached on the server. Defaults to 300.
  , query_is_personal         :: Maybe Bool -- ^ Pass True, if results may be cached on the server side only for the user that sent the query. By default, results may be returned to any user who sends the same query
  , query_next_offset         :: Maybe Text -- ^ Pass the offset that a client should send in the next query with the same text to receive more results. Pass an empty string if there are no more results or if you don‘t support pagination. Offset length can’t exceed 64 bytes.
  , query_switch_pm_text      :: Maybe Text -- ^ If passed, clients will display a button with specified text that switches the user to a private chat with the bot and sends the bot a start message with the parameter switch_pm_parameter
  , query_switch_pm_parameter :: Maybe Text -- ^ Parameter for the start message sent to the bot when user presses the switch button
                                            --
                                            -- Example: An inline bot that sends YouTube videos can ask the user to connect the bot to
                                            -- their YouTube account to adapt search results accordingly. To do this, it displays a
                                            -- ‘Connect your YouTube account’ button above the results, or even before showing any.
                                            -- The user presses the button, switches to a private chat with the bot and, in doing so,
                                            -- passes a start parameter that instructs the bot to return an oauth link. Once done,
                                            -- the bot can offer a switch_inline button so that the user can easily return to the chat where they wanted to use the bot's inline capabilities.
  } deriving (Show, Generic)

instance ToJSON AnswerInlineQueryRequest where
  toJSON = toJsonDrop 6

instance FromJSON AnswerInlineQueryRequest where
  parseJSON = parseJsonDrop 6

answerInlineQueryRequest :: Text -> [InlineQueryResult] -> AnswerInlineQueryRequest
answerInlineQueryRequest queryId results = AnswerInlineQueryRequest queryId results Nothing Nothing Nothing Nothing Nothing

data AnswerCallbackQueryRequest = AnswerCallbackQueryRequest
  {
    cq_callback_query_id :: Text -- ^ Unique identifier for the query to be answered
  , cq_text              :: Maybe Text -- ^ Text of the notification. If not specified, nothing will be shown to the user
  , cq_show_alert        :: Maybe Bool -- ^ If true, an alert will be shown by the client instead of a notification at the top of the chat screen. Defaults to false.
  , cq_url               :: Maybe Text -- ^ URL that will be opened by the user's client. If you have created a `Game` and accepted the conditions via `@Botfather`, specify the URL that opens your game – note that this will only work if the query comes from a `callback_game` button. Otherwise, you may use links like telegram.me/your_bot?start=XXXX that open your bot with a parameter.
  , cq_cache_time        :: Maybe Int -- ^ The maximum amount of time in seconds that the result of the callback query may be cached client-side. Telegram apps will support caching starting in version 3.14. Defaults to 0.
  } deriving (Show, Generic)

instance ToJSON AnswerCallbackQueryRequest where
  toJSON = toJsonDrop 3

instance FromJSON AnswerCallbackQueryRequest where
  parseJSON = parseJsonDrop 3

answerCallbackQueryRequest :: Text -> AnswerCallbackQueryRequest
answerCallbackQueryRequest queryId = AnswerCallbackQueryRequest queryId Nothing Nothing Nothing Nothing

data ReplyKeyboard =
  -- | This object represents a custom keyboard with reply options
  ReplyInlineKeyboardMarkup
  {
    reply_inline_keyboard :: [[InlineKeyboardButton]] -- ^ Array of button rows, each represented by an Array of InlineKeyboardButton objects
  }
  | ReplyKeyboardMarkup
  {
    reply_keyboard          :: [[KeyboardButton]] -- ^ Array of button rows, each represented by an Array of 'KeyboardButton' objects
  , reply_resize_keyboard   :: Maybe Bool -- ^ Requests clients to resize the keyboard vertically for optimal fit (e.g., make the keyboard smaller if there are just two rows of buttons). Defaults to false, in which case the custom keyboard is always of the same height as the app's standard keyboard.
  , reply_one_time_keyboard :: Maybe Bool -- ^ Requests clients to hide the keyboard as soon as it's been used. Defaults to false.
  , reply_selective         :: Maybe Bool -- ^ Use this parameter if you want to show the keyboard to specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply (has reply_to_message_id), sender of the original message.    Example: A user requests to change the bot‘s language, bot replies to the request with a keyboard to select the new language. Other users in the group don’t see the keyboard.
  }
  -- | Upon receiving a message with this object, Telegram clients will hide the current custom keyboard and display the default letter-keyboard. By default, custom keyboards are displayed until a new keyboard is sent by a bot. An exception is made for one-time keyboards that are hidden immediately after the user presses a button
  | ReplyKeyboardHide
  {
    reply_hide_keyboard :: Bool -- ^ Requests clients to hide the custom keyboard
  , reply_selective     :: Maybe Bool -- ^ Use this parameter if you want to hide keyboard for specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply (has reply_to_message_id), sender of the original message.    Example: A user votes in a poll, bot returns confirmation message in reply to the vote and hides keyboard for that user, while still showing the keyboard with poll options to users who haven't voted yet.
  }
  | ReplyKeyboardRemove
  {
    reply_remove_keyboard :: Bool -- ^ Requests clients to remove the custom keyboard (user will not be able to summon this keyboard; if you want to hide the keyboard from sight but keep it accessible, use one_time_keyboard in 'ReplyKeyboardMarkup')
  , reply_selective       :: Maybe Bool -- ^ Use this parameter if you want to hide keyboard for specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply (has reply_to_message_id), sender of the original message.    Example: A user votes in a poll, bot returns confirmation message in reply to the vote and hides keyboard for that user, while still showing the keyboard with poll options to users who haven't voted yet.
  }
  -- | Upon receiving a message with this object, Telegram clients will display a reply interface to the user (act as if the user has selected the bot‘s message and tapped ’Reply'). This can be extremely useful if you want to create user-friendly step-by-step interfaces without having to sacrifice privacy mode.
  | ForceReply
  {
    reply_force_reply :: Bool -- ^ Shows reply interface to the user, as if they manually selected the bot‘s message and tapped ’Reply'
  , reply_selective   :: Maybe Bool -- ^ Use this parameter if you want to force reply from specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply (has reply_to_message_id), sender of the original message.
  } deriving (Show, Generic)

instance ToJSON ReplyKeyboard where
  toJSON = toJsonDrop 6

instance FromJSON ReplyKeyboard where
  parseJSON = parseJsonDrop 6

inlineKeyboardMarkup :: [[InlineKeyboardButton]] -> ReplyKeyboard
inlineKeyboardMarkup = ReplyInlineKeyboardMarkup

replyKeyboardMarkup :: [[KeyboardButton]] -> ReplyKeyboard
replyKeyboardMarkup keyboard = ReplyKeyboardMarkup keyboard Nothing Nothing Nothing

replyKeyboardHide :: ReplyKeyboard
replyKeyboardHide = ReplyKeyboardHide True Nothing

forceReply :: ReplyKeyboard
forceReply = ForceReply True Nothing

data EditMessageTextRequest = EditMessageTextRequest
  {
    emt_chat_id                  :: Maybe ChatId -- ^ Required if `inline_message_id` is not specified. Unique identifier for the target chat or username of the target channel (in the format `@channelusername`)
  , emt_message_id               :: Maybe Int -- ^ if `inline_message_id` is not specified. Unique identifier of the sent message
  , emt_inline_message_id        :: Maybe Text -- ^ Required if chat_id and message_id are not specified. Identifier of the inline message
  , emt_text                     :: Text -- ^ New text of the message
  , emt_parse_mode               :: Maybe ParseMode -- ^ Send `Markdown` or `HTML`, if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , emt_disable_web_page_preview :: Maybe Bool -- ^ Disables link previews for links in this message
  , emt_reply_markup             :: Maybe InlineKeyboardMarkup -- ^ A JSON-serialized object for an inline keyboard.
  } deriving (Show, Generic)

instance ToJSON EditMessageTextRequest where
  toJSON = toJsonDrop 4

instance FromJSON EditMessageTextRequest where
  parseJSON = parseJsonDrop 4

editMessageTextRequest :: ChatId -> Int -> Text -> EditMessageTextRequest
editMessageTextRequest chatId messageId text = EditMessageTextRequest (Just chatId) (Just messageId) Nothing text Nothing Nothing Nothing

editInlineMessageTextRequest :: Text -> Text -> EditMessageTextRequest
editInlineMessageTextRequest inlineMessageId text = EditMessageTextRequest Nothing Nothing (Just inlineMessageId) text Nothing Nothing Nothing

data EditMessageCaptionRequest = EditMessageCaptionRequest
  {
    emc_chat_id           :: Maybe ChatId -- ^ Required if `inline_message_id` is not specified. Unique identifier for the target chat or username of the target channel (in the format `@channelusername`)
  , emc_message_id        :: Maybe Int -- ^ Required if `inline_message_id` is not specified. Unique identifier of the sent message
  , emc_inline_message_id :: Maybe Text -- ^ Required if `chat_id` and `message_id` are not specified. Identifier of the inline message
  , emc_caption           :: Maybe Text -- ^ New caption of the message
  , emc_reply_markup      :: Maybe InlineKeyboardMarkup -- ^ A JSON-serialized object for an inline keyboard.
  } deriving (Show, Generic)

instance ToJSON EditMessageCaptionRequest where
  toJSON = toJsonDrop 4

instance FromJSON EditMessageCaptionRequest where
  parseJSON = parseJsonDrop 4

editMessageCaptionRequest :: ChatId -> Int -> Maybe Text -> EditMessageCaptionRequest
editMessageCaptionRequest chatId messageId caption = EditMessageCaptionRequest (Just chatId) (Just messageId) Nothing caption Nothing

editInlineMessageCaptionRequest :: Text -> Maybe Text -> EditMessageCaptionRequest
editInlineMessageCaptionRequest inlineMessageId caption = EditMessageCaptionRequest Nothing Nothing (Just inlineMessageId) caption Nothing

data EditMessageReplyMarkupRequest = EditMessageReplyMarkupRequest
  {
    emrm_chat_id           :: Maybe ChatId -- ^ Required if `inline_message_id` is not specified. Unique identifier for the target chat or username of the target channel (in the format `@channelusername`)
  , emrm_message_id        :: Maybe Int -- ^ Required if `inline_message_id` is not specified. Unique identifier of the sent message
  , emrm_inline_message_id :: Maybe Text -- ^ Required if `chat_id` and `message_id` are not specified. Identifier of the inline message
  , emrm_reply_markup      :: Maybe InlineKeyboardMarkup -- ^ A JSON-serialized object for an inline keyboard.
  } deriving (Show, Generic)

instance ToJSON EditMessageReplyMarkupRequest where
  toJSON = toJsonDrop 5

instance FromJSON EditMessageReplyMarkupRequest where
  parseJSON = parseJsonDrop 5

editMessageReplyMarkupRequest :: ChatId -> Int -> Maybe InlineKeyboardMarkup -> EditMessageReplyMarkupRequest
editMessageReplyMarkupRequest chatId messageId keyboard = EditMessageReplyMarkupRequest (Just chatId) (Just messageId) Nothing keyboard

editInlineMessageReplyMarkupRequest :: Text -> Maybe InlineKeyboardMarkup -> EditMessageReplyMarkupRequest
editInlineMessageReplyMarkupRequest inlineMessageId keyboard = EditMessageReplyMarkupRequest Nothing Nothing (Just inlineMessageId) keyboard

data SendInvoiceRequest = SendInvoiceRequest
  {
    snd_inv_chat_id                       :: Int64 -- ^ Unique identifier for the target private chat
  , snd_inv_title                         :: Text -- ^ Product name
  , snd_inv_description                   :: Text -- ^ Product description
  , snd_inv_payload                       :: Text -- ^ Bot-defined invoice payload, 1-128 bytes. This will not be displayed to the user, use for your internal processes.
  , snd_inv_provider_token                :: Text -- ^ Payments provider token, obtained via Botfather
  , snd_inv_start_parameter               :: Text -- ^ Unique deep-linking parameter that can be used to generate this invoice when used as a start parameter
  , snd_inv_currency                      :: CurrencyCode -- ^ Three-letter ISO 4217 <https://core.telegram.org/bots/payments#supported-currencies currency> code
  , snd_inv_prices                        :: [LabeledPrice] -- ^ Price breakdown, a list of components (e.g. product price, tax, discount, delivery cost, delivery tax, bonus, etc.)
  , snd_inv_provider_data                 :: Maybe Text -- ^ JSON-encoded data about the invoice, which will be shared with the payment provider. A detailed description of required fields should be provided by the payment provider.
  , snd_inv_photo_url                     :: Maybe Text -- ^ URL of the product photo for the invoice. Can be a photo of the goods or a marketing image for a service. People like it better when they see what they are paying for.
  , snd_inv_photo_size                    :: Maybe Int -- ^ Photo size
  , snd_inv_photo_width                   :: Maybe Int -- ^ Photo width
  , snd_inv_photo_height                  :: Maybe Int -- ^ Photo height
  , snd_inv_need_name                     :: Maybe Bool -- ^ Pass `True`, if you require the user's full name to complete the order
  , snd_inv_need_phone_number             :: Maybe Bool -- ^ Pass `True`, if you require the user's phone number to complete the order
  , snd_inv_need_email                    :: Maybe Bool -- ^ Pass `True`, if you require the user's email to complete the order
  , snd_inv_need_shipping_address         :: Maybe Bool -- ^ Pass `True`, if you require the user's shipping address to complete the order
  , snd_inv_send_phone_number_to_provider :: Maybe Bool -- ^ Pass True, if user's phone number should be sent to provider
  , snd_inv_send_email_to_provider        :: Maybe Bool -- ^ Pass True, if user's email address should be sent to provider
  , snd_inv_is_flexible                   :: Maybe Bool -- ^ Pass `True`, if the final price depends on the shipping method
  , snd_inv_disable_notification          :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , snd_inv_reply_to_message              :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , snd_inv_reply_markup                  :: Maybe InlineKeyboardMarkup -- ^ A JSON-serialized object for an inline keyboard. If empty, one 'Pay total price' button will be shown. If not empty, the first button must be a Pay button.
  } deriving (Show, Generic)

instance ToJSON SendInvoiceRequest where
  toJSON = toJsonDrop 8

instance FromJSON SendInvoiceRequest where
  parseJSON = parseJsonDrop 8

sendInvoiceRequest :: Int64 -- ^ Unique identifier for the target private chat
  -> Text -- ^ Product name
  -> Text -- ^ Product description
  -> Text -- ^ Bot-defined invoice payload, 1-128 bytes. This will not be displayed to the user, use for your internal processes.
  -> Text -- ^ Payments provider token, obtained via Botfather
  -> Text -- ^ Unique deep-linking parameter that can be used to generate this invoice when used as a start parameter
  -> CurrencyCode -- ^ Three-letter ISO 4217 <https://core.telegram.org/bots/payments#supported-currencies currency> code
  -> [LabeledPrice] -- ^ Price breakdown, a list of components (e.g. product price, tax, discount, delivery cost, delivery tax, bonus, etc.)
  -> SendInvoiceRequest
sendInvoiceRequest chatId title description payload providerToken startParameter currency prices
  = SendInvoiceRequest chatId title description payload providerToken startParameter currency prices Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data AnswerShippingQueryRequest
  -- | If you sent an invoice requesting a shipping address and the parameter is_flexible was specified, the Bot API will send an Update with a shipping_query field to the bot. Use this method to reply to shipping queries. On success, True is returned.
  = AnswerShippingQueryRequest
  {
    asq_shipping_query_id :: Text -- ^ Unique identifier for the query to be answered
  , asq_ok                :: Bool -- ^ Specify True if delivery to the specified address is possible and False if there are any problems (for example, if delivery to the specified address is not possible)
  , asq_shipping_options  :: Maybe [ShippingOption] -- ^ Required if ok is True. A JSON-serialized array of available shipping options.
  , asq_error_message     :: Maybe Text -- ^ Required if ok is False. Error message in human readable form that explains why it is impossible to complete the order (e.g. "Sorry, delivery to your desired address is unavailable'). Telegram will display this message to the user.
  } deriving (Show, Generic)

instance ToJSON AnswerShippingQueryRequest where
  toJSON = toJsonDrop 4

instance FromJSON AnswerShippingQueryRequest where
  parseJSON = parseJsonDrop 4

okShippingQueryRequest :: Text -> [ShippingOption] -> AnswerShippingQueryRequest
okShippingQueryRequest queryId options = AnswerShippingQueryRequest queryId True (Just options) Nothing

errorShippingQueryRequest :: Text -> Text -> AnswerShippingQueryRequest
errorShippingQueryRequest queryId errorMsg = AnswerShippingQueryRequest queryId False Nothing (Just errorMsg)

data AnswerPreCheckoutQueryRequest
  -- | Once the user has confirmed their payment and shipping details, the Bot API sends the final confirmation in the form of an `Update` with the field pre_checkout_query. Use this method to respond to such pre-checkout queries. On success, True is returned. Note: The Bot API must receive an answer within 10 seconds after the pre-checkout query was sent.
  = AnswerPreCheckoutQueryRequest
  {
    apc_pre_checkout_query_id :: Text -- ^ 	Unique identifier for the query to be answered
  , apc_ok                    :: Bool -- ^ Specify True if everything is alright (goods are available, etc.) and the bot is ready to proceed with the order. Use False if there are any problems.
  , apc_error_message         :: Maybe Text -- ^ Required if ok is False. Error message in human readable form that explains the reason for failure to proceed with the checkout (e.g. "Sorry, somebody just bought the last of our amazing black T-shirts while you were busy filling out your payment details. Please choose a different color or garment!"). Telegram will display this message to the user.
  } deriving (Show, Generic)

instance ToJSON AnswerPreCheckoutQueryRequest where
  toJSON = toJsonDrop 4

instance FromJSON AnswerPreCheckoutQueryRequest where
  parseJSON = parseJsonDrop 4

okAnswerPrecheckoutQueryRequest :: Text -> AnswerPreCheckoutQueryRequest
okAnswerPrecheckoutQueryRequest queryId = AnswerPreCheckoutQueryRequest queryId True Nothing

errorAnswerPrecheckoutQueryRequest :: Text -> Text -> AnswerPreCheckoutQueryRequest
errorAnswerPrecheckoutQueryRequest queryId errorMessage = AnswerPreCheckoutQueryRequest queryId False $ Just errorMessage

data RestrictChatMemberRequest = RestrictChatMemberRequest
  {
    rcm_chat_id                   :: ChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  , rcm_user_id                   :: Int -- ^ Unique identifier of the target user
  , rcm_until_date                :: Maybe Int -- ^ Date when restrictions will be lifted for the user, unix time. If user is restricted for more than 366 days or less than 30 seconds from the current time, they are considered to be restricted forever
  , rcm_can_send_messages         :: Maybe Bool -- ^ Pass True, if the user can send text messages, contacts, locations and venues
  , rcm_can_send_media_messages   :: Maybe Bool -- ^ Pass True, if the user can send audios, documents, photos, videos, video notes and voice notes, implies can_send_messages
  , rcm_can_send_other_messages   :: Maybe Bool -- ^ Pass True, if the user can send animations, games, stickers and use inline bots, implies can_send_media_messages
  , rcm_can_add_web_page_previews :: Maybe Bool -- ^ Pass True, if the user may add web page previews to their messages, implies can_send_media_messages
  } deriving (Show, Generic)

instance ToJSON RestrictChatMemberRequest where
  toJSON = toJsonDrop 4

instance FromJSON RestrictChatMemberRequest where
  parseJSON = parseJsonDrop 4

restrictChatMemberRequest :: ChatId -> Int -> RestrictChatMemberRequest
restrictChatMemberRequest chatId userId = RestrictChatMemberRequest chatId userId Nothing Nothing Nothing Nothing Nothing

data PromoteChatMemberRequest = PromoteChatMemberRequest
  {
    pcmr_chat_id              :: ChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , pcmr_user_id              :: Int -- ^ Unique identifier of the target user
  , pcmr_can_change_info      :: Maybe Bool -- ^ Pass True, if the administrator can change chat title, photo and other settings
  , pcmr_can_post_messages    :: Maybe Bool -- ^ Pass True, if the administrator can create channel posts, channels only
  , pcmr_can_edit_messages    :: Maybe Bool -- ^ Pass True, if the administrator can edit messages of other users and can pin messages, channels only
  , pcmr_can_delete_messages  :: Maybe Bool -- ^ Pass True, if the administrator can delete messages of other users
  , pcmr_can_invite_users     :: Maybe Bool -- ^ Pass True, if the administrator can invite new users to the chat
  , pcmr_can_restrict_members :: Maybe Bool -- ^ Pass True, if the administrator can restrict, ban or unban chat members
  , pcmr_can_pin_messages     :: Maybe Bool -- ^ Pass True, if the administrator can pin messages, supergroups only
  , pcmr_can_promote_members  :: Maybe Bool -- ^ Pass True, if the administrator can add new administrators with a subset of his own privileges or demote administrators that he has promoted, directly or indirectly (promoted by administrators that were appointed by him)
  } deriving (Show, Generic)

instance ToJSON PromoteChatMemberRequest where
  toJSON = toJsonDrop 5

instance FromJSON PromoteChatMemberRequest where
  parseJSON = parseJsonDrop 5

promoteChatMemberRequest :: ChatId -> Int -> PromoteChatMemberRequest
promoteChatMemberRequest chatId userId = PromoteChatMemberRequest chatId userId Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data SetChatPhotoRequest = SetChatPhotoRequest
  {
    scp_chat_id :: ChatId
  , scp_photo   :: FileUpload
  }

instance ToMultipartFormData SetChatPhotoRequest where
  toMultipartFormData req =
    [ utf8Part "chat_id" (chatIdToPart $ scp_chat_id req)
    , fileUploadToPart "photo" (scp_photo req) ]

data UploadStickerFileRequest = UploadStickerFileRequest
  {
    upload_sticker_user_id     :: Int -- ^ User identifier of sticker file owner
  , upload_sticker_png_sticker :: FileUpload -- ^ Png image with the sticker, must be up to 512 kilobytes in size, dimensions must not exceed 512px, and either width or height must be exactly 512px.
  }

instance ToMultipartFormData UploadStickerFileRequest where
  toMultipartFormData req =
    [ utf8Part "user_id" (tshow $ upload_sticker_user_id req)
    , fileUploadToPart "png_sticker" (upload_sticker_png_sticker req) ]

data CreateNewStickerSetRequest payload = CreateNewStickerSetRequest
  {
    new_sticker_set_user_id        :: Int -- ^ User identifier of created sticker set owner
  , new_sticker_set_name           :: Text -- ^ Short name of sticker set, to be used in t.me/addstickers/ URLs (e.g., animals). Can contain only english letters, digits and underscores. Must begin with a letter, can't contain consecutive underscores and must end in “_by_<bot username>”. <bot_username> is case insensitive. 1-64 characters.
  , new_sticker_set_title          :: Text -- ^ Sticker set title, 1-64 characters
  , new_sticker_set_png_sticker    :: payload -- ^ Yes 	Png image with the sticker, must be up to 512 kilobytes in size, dimensions must not exceed 512px, and either width or height must be exactly 512px. Pass a file_id as a String to send a file that already exists on the Telegram servers, pass an HTTP URL as a String for Telegram to get a file from the Internet, or upload a new one using multipart/form-data. More info on Sending Files »
  , new_sticker_set_emojis         :: Text -- ^ One or more emoji corresponding to the sticker
  , new_sticker_set_contains_masks :: Maybe Bool -- ^ Pass True, if a set of mask stickers should be created
  , new_sticker_set_mask_position  :: Maybe MaskPosition -- ^ A JSON-serialized object for position where the mask should be placed on faces
  } deriving (Show, Generic)

instance ToJSON (CreateNewStickerSetRequest Text) where
  toJSON = toJsonDrop 16

instance FromJSON (CreateNewStickerSetRequest Text) where
  parseJSON = parseJsonDrop 16

instance ToMultipartFormData (CreateNewStickerSetRequest FileUpload) where
  toMultipartFormData req =
    [ utf8Part "user_id" (tshow $ new_sticker_set_user_id req)
    , utf8Part "name" $ new_sticker_set_name req
    , utf8Part "title" $ new_sticker_set_title req
    , utf8Part "emojis" $ new_sticker_set_emojis req
    , fileUploadToPart "png_sticker" (new_sticker_set_png_sticker req) ] ++
    catMaybes
    [ partLBS "contains_masks" . encode <$> new_sticker_set_contains_masks req
    , partLBS "mask_position" . encode <$> new_sticker_set_mask_position req ]

data AddStickerToSetRequest payload = AddStickerToSetRequest
  {
    add_sticker_to_set_user_id       :: Int -- ^ User identifier of created sticker set owner
  , add_sticker_to_set_name          :: Text -- ^ Short name of sticker set, to be used in t.me/addstickers/ URLs (e.g., animals). Can contain only english letters, digits and underscores. Must begin with a letter, can't contain consecutive underscores and must end in “_by_<bot username>”. <bot_username> is case insensitive. 1-64 characters.
  , add_sticker_to_set_png_sticker   :: payload -- ^ Yes 	Png image with the sticker, must be up to 512 kilobytes in size, dimensions must not exceed 512px, and either width or height must be exactly 512px. Pass a file_id as a String to send a file that already exists on the Telegram servers, pass an HTTP URL as a String for Telegram to get a file from the Internet, or upload a new one using multipart/form-data. More info on Sending Files »
  , add_sticker_to_set_emojis        :: Text -- ^ One or more emoji corresponding to the sticker
  , add_sticker_to_set_mask_position :: Maybe MaskPosition -- ^ A JSON-serialized object for position where the mask should be placed on faces
  } deriving (Show, Generic)

instance ToJSON (AddStickerToSetRequest Text) where
  toJSON = toJsonDrop 19

instance FromJSON (AddStickerToSetRequest Text) where
  parseJSON = parseJsonDrop 19

instance ToMultipartFormData (AddStickerToSetRequest FileUpload) where
  toMultipartFormData req =
    [ utf8Part "user_id" (tshow $ add_sticker_to_set_user_id req)
    , utf8Part "name" $ add_sticker_to_set_name req
    , utf8Part "emojis" $ add_sticker_to_set_emojis req
    , fileUploadToPart "png_sticker" (add_sticker_to_set_png_sticker req) ] ++
    catMaybes
    [ partLBS "mask_position" . encode <$> add_sticker_to_set_mask_position req
    ]

data EditMessageLiveLocationRequest =
  EditMessageLiveLocationRequest
  {
    edit_live_loc_chat_id      :: Text -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , edit_live_loc_latitude     :: Float -- ^ Latitude of new location
  , edit_live_loc_longitude    :: Float -- ^ Longitude of new location
  , edit_live_loc_reply_markup :: Maybe InlineKeyboardMarkup -- ^ An object for a new inline keyboard.
  }
  | EditMessageLiveLocationMessageRequest
  {
    edit_live_loc_message_id   :: Int -- ^ Identifier of the sent message
  , edit_live_loc_latitude     :: Float -- ^ Latitude of new location
  , edit_live_loc_longitude    :: Float -- ^ Longitude of new location
  , edit_live_loc_reply_markup :: Maybe InlineKeyboardMarkup -- ^ An object for a new inline keyboard.
  }
  | EditMessageLiveLocationInlineMessageRequest
  {
    edit_live_loc_inline_message_id :: Text -- ^ Identifier of the inline message
  , edit_live_loc_latitude          :: Float -- ^ Latitude of new location
  , edit_live_loc_longitude         :: Float -- ^ Longitude of new location
  , edit_live_loc_reply_markup      :: Maybe InlineKeyboardMarkup -- ^ An object for a new inline keyboard.
  } deriving (Show, Generic)

instance ToJSON EditMessageLiveLocationRequest where
  toJSON = toJsonDrop 14

instance FromJSON EditMessageLiveLocationRequest where
  parseJSON = parseJsonDrop 14

data StopMessageLiveLocationRequest =
  StopMessageLiveLocationRequest
  {
    stop_live_loc_chat_id      :: Text -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , stop_live_loc_reply_markup :: Maybe InlineKeyboardMarkup -- ^ An object for a new inline keyboard.
  }
  | StopMessageLiveLocationMessageRequest
  {
    stop_live_loc_message_id   :: Int -- ^ Identifier of the sent message
  , stop_live_loc_reply_markup :: Maybe InlineKeyboardMarkup -- ^ An object for a new inline keyboard.
  }
  | StopMessageLiveLocationInlineMessageRequest
  {
    stop_live_loc_inline_message_id :: Text -- ^ Identifier of the inline message
  , stop_live_loc_reply_markup      :: Maybe InlineKeyboardMarkup -- ^ An object for a new inline keyboard.
  } deriving (Show, Generic)

instance ToJSON StopMessageLiveLocationRequest where
  toJSON = toJsonDrop 14

instance FromJSON StopMessageLiveLocationRequest where
  parseJSON = parseJsonDrop 14

tshow :: Show a => a -> Text
tshow = T.pack . show
