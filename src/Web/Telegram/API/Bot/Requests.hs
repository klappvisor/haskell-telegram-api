{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- | This module contains data objects which represents requests to Telegram Bot API
module Web.Telegram.API.Bot.Requests
    ( -- * Types
      SendMessageRequest             (..)
    , ForwardMessageRequest          (..)
    , FileUpload                     (..)
    , FileUploadContent              (..)
    , SendPhotoRequest               (..)
    , SendAudioRequest               (..)
    , SendDocumentRequest            (..)
    , SendStickerRequest             (..)
    , SendVideoRequest               (..)
    , SendVoiceRequest               (..)
    , SendLocationRequest            (..)
    , SendVenueRequest               (..)
    , SendContactRequest             (..)
    , SendChatActionRequest          (..)
    , ChatAction                     (..)
    , AnswerInlineQueryRequest       (..)
    , AnswerCallbackQueryRequest     (..)
    , ReplyKeyboard                  (..)
    , EditMessageTextRequest         (..)
    , EditMessageCaptionRequest      (..)
    , EditMessageReplyMarkupRequest  (..)
     -- * Functions
    , localFileUpload
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
    , sendLocationRequest
    , sendVenueRequest
    , sendContactRequest
    , sendChatActionRequest
    , answerInlineQueryRequest
    , answerCallbackQueryRequest
    , replyKeyboardMarkup
    , replyKeyboardHide
    , forceReply
    , editMessageTextRequest
    , editInlineMessageTextRequest
    , editMessageCaptionRequest
    , editInlineMessageCaptionRequest
    , editMessageReplyMarkupRequest
    , editInlineMessageReplyMarkupRequest
    ) where

import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           GHC.Generics
import           Network.HTTP.Client.MultipartFormData
import           Network.Mime
import           Servant.Client.MultipartFormData (ToMultipartFormData (..))
import           Web.Telegram.API.Bot.JsonExt
import           Web.Telegram.API.Bot.Data


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
localFileUpload path =
  FileUpload
  { fileUpload_type = Nothing
  , fileUpload_content = FileUploadFile path
  }

fileUploadToPart :: Text -> FileUpload -> Part
fileUploadToPart inputName fileUpload =
  let part =
        case fileUpload_content fileUpload of
          FileUploadFile path -> partFileSource inputName path
          FileUploadBS bs -> partBS inputName bs
          FileUploadLBS lbs -> partLBS inputName lbs
  in part { partContentType = fileUpload_type fileUpload }

utf8Part :: Text -> Text -> Part
utf8Part inputName = partBS inputName . T.encodeUtf8

-- | This object represents request for 'sendMessage'
data SendMessageRequest = SendMessageRequest
  {
    message_chat_id                  :: Text -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
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

sendMessageRequest :: Text -> Text -> SendMessageRequest
sendMessageRequest chatId text = SendMessageRequest chatId text Nothing Nothing Nothing Nothing Nothing

-- | This object represents request for 'forwardMessage'
data ForwardMessageRequest = ForwardMessageRequest
  {
    forward_chat_id :: Text -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , forward_from_chat_id :: Text -- ^ Unique identifier for the chat where the original message was sent (or channel username in the format @@channelusername@)
  , forward_disable_notification     :: Maybe Bool -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , forward_message_id :: Int -- ^ Unique message identifier
  } deriving (Show, Generic)

instance ToJSON ForwardMessageRequest where
  toJSON = toJsonDrop 8

instance FromJSON ForwardMessageRequest where
  parseJSON = parseJsonDrop 8

forwardMessageRequest :: Text -> Text -> Int -> ForwardMessageRequest
forwardMessageRequest chatId fromChatId forwardMessageId = ForwardMessageRequest chatId fromChatId Nothing forwardMessageId

-- | This object represents request for 'sendPhoto'
data SendPhotoRequest payload = SendPhotoRequest
  {
    photo_chat_id              :: Text -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
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

sendPhotoRequest :: Text -> Text -> SendPhotoRequest Text
sendPhotoRequest chatId photo = SendPhotoRequest chatId photo Nothing Nothing Nothing Nothing

uploadPhotoRequest :: Text -> FileUpload -> SendPhotoRequest FileUpload
uploadPhotoRequest chatId photo = SendPhotoRequest chatId photo Nothing Nothing Nothing Nothing

instance ToMultipartFormData (SendPhotoRequest FileUpload) where
  toMultipartFormData req =
    [ utf8Part "chat_id" (photo_chat_id req) ] ++
    catMaybes
    [ utf8Part "caption" <$> photo_caption req
    , partLBS "disable_notification" . encode <$> photo_disable_notification req
    , utf8Part "reply_to_message_id" . T.pack . show <$> photo_reply_to_message_id req
    , partLBS "reply_markup" . encode <$> photo_reply_markup req
    ] ++
    [ fileUploadToPart "photo" (photo_photo req) ]

-- | This object represents request for 'sendAudio'
data SendAudioRequest payload = SendAudioRequest
  {
    _audio_chat_id              :: Text -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , _audio_audio                :: payload -- ^ Audio to send. You can either pass a file_id as String to resend an audio that is already on the Telegram servers, or upload a new audio file.
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
    [ utf8Part "chat_id" (_audio_chat_id req) ] ++
    catMaybes
    [ utf8Part "duration" . T.pack . show <$> _audio_duration req
    , utf8Part "performer" <$> _audio_performer req
    , utf8Part "title" <$> _audio_title req
    , partLBS "disable_notification" . encode <$> _audio_disable_notification req
    , utf8Part "reply_to_message_id" . T.pack . show <$> _audio_reply_to_message_id req
    , partLBS "reply_markup" . encode <$> _audio_reply_markup req
    ] ++
    [ fileUploadToPart "audio" (_audio_audio req) ]

sendAudioRequest :: Text -> Text -> SendAudioRequest Text
sendAudioRequest chatId audio = SendAudioRequest chatId audio Nothing Nothing Nothing Nothing Nothing Nothing

uploadAudioRequest :: Text -> FileUpload -> SendAudioRequest FileUpload
uploadAudioRequest chatId audio = SendAudioRequest chatId audio Nothing Nothing Nothing Nothing Nothing Nothing

-- | This object represents request for 'sendSticker'
data SendStickerRequest payload = SendStickerRequest
  {
    sticker_chat_id                  :: Text -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , sticker_sticker                  :: payload -- ^ Sticker to send. You can either pass a file_id as String to resend a sticker that is already on the Telegram servers, or upload a new sticker.
  , sticker_disable_notification     :: Maybe Bool -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , sticker_reply_to_message_id      :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , sticker_reply_markup             :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Show, Generic)

instance ToJSON (SendStickerRequest Text) where
  toJSON = toJsonDrop 8

instance FromJSON (SendStickerRequest Text) where
  parseJSON = parseJsonDrop 8

instance ToMultipartFormData (SendStickerRequest FileUpload) where
  toMultipartFormData req =
    [ utf8Part "chat_id" (sticker_chat_id req) ] ++
    catMaybes
    [ partLBS "disable_notification" . encode <$> sticker_disable_notification req
    , utf8Part "reply_to_message_id" . T.pack . show <$> sticker_reply_to_message_id req
    , partLBS "reply_markup" . encode <$> sticker_reply_markup req
    ] ++
    [ fileUploadToPart "sticker" (sticker_sticker req) ]

sendStickerRequest :: Text -> Text -> SendStickerRequest Text
sendStickerRequest chatId sticker = SendStickerRequest chatId sticker Nothing Nothing Nothing

uploadStickerRequest :: Text -> FileUpload -> SendStickerRequest FileUpload
uploadStickerRequest chatId sticker = SendStickerRequest chatId sticker Nothing Nothing Nothing

-- | This object represents request for 'sendDocument'
data SendDocumentRequest payload = SendDocumentRequest
  {
    document_chat_id                  :: Text -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , document_document                 :: payload -- ^ File to send. You can either pass a file_id as String to resend a file that is already on the Telegram servers, or upload a new file.
  , document_caption                  :: Maybe Text -- ^ Document caption (may also be used when resending documents by file_id), 0-200 characters
  , document_disable_notification     :: Maybe Bool -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , document_reply_to_message_id      :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , document_reply_markup             :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Show, Generic)

instance ToJSON (SendDocumentRequest Text) where
  toJSON = toJsonDrop 9

instance FromJSON (SendDocumentRequest Text) where
  parseJSON = parseJsonDrop 9

instance ToMultipartFormData (SendDocumentRequest FileUpload) where
  toMultipartFormData req =
    [ utf8Part "chat_id" (document_chat_id req) ] ++
    catMaybes
    [ utf8Part "caption" <$> document_caption req
    , partLBS "disable_notification" . encode <$> document_disable_notification req
    , utf8Part "reply_to_message_id" . T.pack . show <$> document_reply_to_message_id req
    , partLBS "reply_markup" . encode <$> document_reply_markup req
    ] ++
    [ fileUploadToPart "document" (document_document req) ]

sendDocumentRequest :: Text -> Text -> SendDocumentRequest Text
sendDocumentRequest chatId document = SendDocumentRequest chatId document Nothing Nothing Nothing Nothing

uploadDocumentRequest :: Text -> FileUpload -> SendDocumentRequest FileUpload
uploadDocumentRequest chatId document = SendDocumentRequest chatId document Nothing Nothing Nothing Nothing

-- | This object represents request for 'sendVideo'
data SendVideoRequest payload = SendVideoRequest
  {
    _video_chat_id                  :: Text -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , _video_video                    :: payload -- ^ Video to send. You can either pass a file_id as String to resend a video that is already on the Telegram servers, or upload a new video.
  , _video_duration                 :: Maybe Int -- ^ Duration of sent video in seconds
  , _video_caption                  :: Maybe Text -- ^ Video caption, 0-200 characters.
  , _video_disable_notification     :: Maybe Bool -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , _video_reply_to_message_id      :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , _video_reply_markup             :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Show, Generic)

instance ToJSON (SendVideoRequest Text) where
  toJSON = toJsonDrop 7

instance FromJSON (SendVideoRequest Text) where
  parseJSON = parseJsonDrop 7

instance ToMultipartFormData (SendVideoRequest FileUpload) where
  toMultipartFormData req =
    [ utf8Part "chat_id" (_video_chat_id req) ] ++
    catMaybes
    [ partLBS "duration" . encode <$> _video_duration req
    , utf8Part "caption" <$> _video_caption req
    , partLBS "disable_notification" . encode <$> _video_disable_notification req
    , utf8Part "reply_to_message_id" . T.pack . show <$> _video_reply_to_message_id req
    , partLBS "reply_markup" . encode <$> _video_reply_markup req
    ] ++
    [ fileUploadToPart "video" (_video_video req) ]

sendVideoRequest :: Text -> Text -> SendVideoRequest Text
sendVideoRequest chatId video = SendVideoRequest chatId video Nothing Nothing Nothing Nothing Nothing

uploadVideoRequest :: Text -> FileUpload -> SendVideoRequest FileUpload
uploadVideoRequest chatId video = SendVideoRequest chatId video Nothing Nothing Nothing Nothing Nothing

-- | This object represents request for 'sendVoice'
data SendVoiceRequest payload = SendVoiceRequest
  {
    _voice_chat_id                  :: Text -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , _voice_voice                    :: payload -- ^ Audio file to send. You can either pass a file_id as String to resend an audio that is already on the Telegram servers, or upload a new audio file.
  , _voice_duration                 :: Maybe Int -- ^ Duration of sent audio in seconds
  , _voice_disable_notification     :: Maybe Bool -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , _voice_reply_to_message_id      :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , _voice_reply_markup             :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Show, Generic)

instance ToJSON (SendVoiceRequest Text) where
  toJSON = toJsonDrop 7

instance FromJSON (SendVoiceRequest Text) where
  parseJSON = parseJsonDrop 7

instance ToMultipartFormData (SendVoiceRequest FileUpload) where
  toMultipartFormData req =
    [ utf8Part "chat_id" (_voice_chat_id req) ] ++
    catMaybes
    [ partLBS "duration" . encode <$> _voice_duration req
    , partLBS "disable_notification" . encode <$> _voice_disable_notification req
    , utf8Part "reply_to_message_id" . T.pack . show <$> _voice_reply_to_message_id req
    , partLBS "reply_markup" . encode <$> _voice_reply_markup req
    ] ++
    [ fileUploadToPart "voice" (_voice_voice req) ]

sendVoiceRequest :: Text -> Text -> SendVoiceRequest Text
sendVoiceRequest chatId voice = SendVoiceRequest chatId voice Nothing Nothing Nothing Nothing

uploadVoiceRequest :: Text -> FileUpload -> SendVoiceRequest FileUpload
uploadVoiceRequest chatId voice = SendVoiceRequest chatId voice Nothing Nothing Nothing Nothing

-- | This object represents request for 'sendLocation'
data SendLocationRequest = SendLocationRequest
  {
    location_chat_id                :: Text -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , location_latitude               :: Float -- ^ Latitude of location
  , location_longitude              :: Float -- ^ Longitude of location
  , location_disable_notification   :: Maybe Bool -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , location_reply_to_message_id    :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , location_reply_markup           :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Show, Generic)

instance ToJSON SendLocationRequest where
  toJSON = toJsonDrop 9

instance FromJSON SendLocationRequest where
  parseJSON = parseJsonDrop 9

sendLocationRequest :: Text -> Float -> Float -> SendLocationRequest
sendLocationRequest chatId latitude longitude = SendLocationRequest chatId latitude longitude Nothing Nothing Nothing

-- | This object represents request for 'sendVenue'
data SendVenueRequest = SendVenueRequest
  {
    _venue_chat_id               :: Text -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , _venue_latitude              :: Float -- ^ Latitude of the venue
  , _venue_longitude             :: Float -- ^ Longitude of the venue
  , _venue_title                 :: Text -- ^ Name of the venue
  , _venue_address               :: Text -- ^ Address of the venue
  , _venue_foursquare_id         :: Maybe Text -- ^ Foursquare identifier of the venue
  , _venue_disable_notification  :: Maybe Bool -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , _venue_reply_to_message_id   :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , _venue_reply_markup          :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Show, Generic)

instance ToJSON SendVenueRequest where
  toJSON = toJsonDrop 7

instance FromJSON SendVenueRequest where
  parseJSON = parseJsonDrop 7

sendVenueRequest :: Text -> Float -> Float -> Text -> Text -> SendVenueRequest
sendVenueRequest chatId latitude longitude title address = SendVenueRequest chatId latitude longitude title address Nothing Nothing Nothing Nothing

-- | This object represents request for 'sendContact'
data SendContactRequest = SendContactRequest
  {
    _contact_chat_id              :: Text -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
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

sendContactRequest :: Text -> Text -> Text -> SendContactRequest
sendContactRequest chatId phoneNumber firstName = SendContactRequest chatId phoneNumber firstName Nothing Nothing Nothing Nothing

-- | Type of action to broadcast.
data ChatAction = Typing
                | UploadPhoto
                | RecordVideo
                | UploadVideo
                | RecordAudio
                | UploadAudio
                | UploadDocument
                | FindLocation deriving (Show, Generic)

instance ToJSON ChatAction where
  toJSON Typing         = "typing"
  toJSON UploadPhoto    = "upload_photo"
  toJSON RecordVideo    = "record_video"
  toJSON UploadVideo    = "upload_video"
  toJSON RecordAudio    = "record_audio"
  toJSON UploadAudio    = "upload_audio"
  toJSON UploadDocument = "upload_document"
  toJSON FindLocation   = "find_location"

instance FromJSON ChatAction where
  parseJSON "typing"          = pure Typing
  parseJSON "upload_photo"    = pure UploadPhoto
  parseJSON "record_video"    = pure RecordVideo
  parseJSON "upload_video"    = pure UploadVideo
  parseJSON "record_audio"    = pure RecordAudio
  parseJSON "upload_audio"    = pure UploadAudio
  parseJSON "upload_document" = pure UploadDocument
  parseJSON "find_location"   = pure FindLocation
  parseJSON _                 = fail "Failed to parse ChatAction"

-- | This object represents request for 'sendChatAction'
data SendChatActionRequest = SendChatActionRequest
  {
    action_chat_id :: Text -- ^ Unique identifier for the target chat or username of the target channel (in the format `@channelusername`)
  , action_action :: ChatAction -- ^ Type of action to broadcast.
  } deriving (Show, Generic)

instance ToJSON SendChatActionRequest where
  toJSON = toJsonDrop 7

instance FromJSON SendChatActionRequest where
  parseJSON = parseJsonDrop 7

sendChatActionRequest :: Text -> ChatAction -> SendChatActionRequest
sendChatActionRequest = SendChatActionRequest

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
  , cq_text :: Maybe Text -- ^ Text of the notification. If not specified, nothing will be shown to the user
  , cq_show_alert :: Maybe Bool -- ^ If true, an alert will be shown by the client instead of a notification at the top of the chat screen. Defaults to false.
  } deriving (Show, Generic)

instance ToJSON AnswerCallbackQueryRequest where
  toJSON = toJsonDrop 3

instance FromJSON AnswerCallbackQueryRequest where
  parseJSON = parseJsonDrop 3

answerCallbackQueryRequest :: Text -> AnswerCallbackQueryRequest
answerCallbackQueryRequest chatId = AnswerCallbackQueryRequest chatId Nothing Nothing

data ReplyKeyboard =
  -- | This object represents a custom keyboard with reply options
  ReplyKeyboardMarkup
  {
    reply_keyboard             :: [[KeyboardButton]] -- ^ Array of button rows, each represented by an Array of 'KeyboardButton' objects
  , reply_resize_keyboard      :: Maybe Bool -- ^ Requests clients to resize the keyboard vertically for optimal fit (e.g., make the keyboard smaller if there are just two rows of buttons). Defaults to false, in which case the custom keyboard is always of the same height as the app's standard keyboard.
  , reply_one_time_keyboard    :: Maybe Bool -- ^ Requests clients to hide the keyboard as soon as it's been used. Defaults to false.
  , reply_selective            :: Maybe Bool -- ^ Use this parameter if you want to show the keyboard to specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply (has reply_to_message_id), sender of the original message.    Example: A user requests to change the bot‘s language, bot replies to the request with a keyboard to select the new language. Other users in the group don’t see the keyboard.
  }
  -- | Upon receiving a message with this object, Telegram clients will hide the current custom keyboard and display the default letter-keyboard. By default, custom keyboards are displayed until a new keyboard is sent by a bot. An exception is made for one-time keyboards that are hidden immediately after the user presses a button
  | ReplyKeyboardHide
  {
    reply_hide_keyboard        :: Bool -- ^ Requests clients to hide the custom keyboard
  , reply_selective            :: Maybe Bool -- ^ Use this parameter if you want to hide keyboard for specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply (has reply_to_message_id), sender of the original message.    Example: A user votes in a poll, bot returns confirmation message in reply to the vote and hides keyboard for that user, while still showing the keyboard with poll options to users who haven't voted yet.
  }
  -- | Upon receiving a message with this object, Telegram clients will display a reply interface to the user (act as if the user has selected the bot‘s message and tapped ’Reply'). This can be extremely useful if you want to create user-friendly step-by-step interfaces without having to sacrifice privacy mode.
  | ForceReply
  {
    reply_force_reply          :: Bool -- ^ Shows reply interface to the user, as if they manually selected the bot‘s message and tapped ’Reply'
  , reply_selective            :: Maybe Bool -- ^ Use this parameter if you want to force reply from specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply (has reply_to_message_id), sender of the original message.
  } deriving (Show, Generic)

instance ToJSON ReplyKeyboard where
  toJSON = toJsonDrop 6

instance FromJSON ReplyKeyboard where
  parseJSON = parseJsonDrop 6

replyKeyboardMarkup :: [[KeyboardButton]] -> ReplyKeyboard
replyKeyboardMarkup keyboard = ReplyKeyboardMarkup keyboard Nothing Nothing Nothing

replyKeyboardHide :: ReplyKeyboard
replyKeyboardHide = ReplyKeyboardHide True Nothing

forceReply :: ReplyKeyboard
forceReply = ForceReply True Nothing

data EditMessageTextRequest = EditMessageTextRequest
  {
    emt_chat_id :: Maybe Text -- ^ Required if `inline_message_id` is not specified. Unique identifier for the target chat or username of the target channel (in the format `@channelusername`)
  , emt_message_id :: Maybe Int -- ^ if `inline_message_id` is not specified. Unique identifier of the sent message
  , emt_inline_message_id :: Maybe Text -- ^ Required if chat_id and message_id are not specified. Identifier of the inline message
  , emt_text :: Text -- ^ New text of the message
  , emt_parse_mode :: Maybe ParseMode -- ^ Send `Markdown` or `HTML`, if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , emt_disable_web_page_preview :: Maybe Bool -- ^ Disables link previews for links in this message
  , emt_reply_markup :: Maybe InlineKeyboardMarkup -- ^ A JSON-serialized object for an inline keyboard.
  } deriving (Show, Generic)

instance ToJSON EditMessageTextRequest where
  toJSON = toJsonDrop 4

instance FromJSON EditMessageTextRequest where
  parseJSON = parseJsonDrop 4

editMessageTextRequest :: Text -> Int -> Text -> EditMessageTextRequest
editMessageTextRequest chatId messageId text = EditMessageTextRequest (Just chatId) (Just messageId) Nothing text Nothing Nothing Nothing

editInlineMessageTextRequest :: Text -> Text -> EditMessageTextRequest
editInlineMessageTextRequest inlineMessageId text = EditMessageTextRequest Nothing Nothing (Just inlineMessageId) text Nothing Nothing Nothing

data EditMessageCaptionRequest = EditMessageCaptionRequest
  {
    emc_chat_id :: Maybe Text -- ^ Required if `inline_message_id` is not specified. Unique identifier for the target chat or username of the target channel (in the format `@channelusername`)
  , emc_message_id :: Maybe Int -- ^ Required if `inline_message_id` is not specified. Unique identifier of the sent message
  , emc_inline_message_id :: Maybe Text -- ^ Required if `chat_id` and `message_id` are not specified. Identifier of the inline message
  , emc_caption :: Maybe Text -- ^ New caption of the message
  , emc_reply_markup :: Maybe InlineKeyboardMarkup -- ^ A JSON-serialized object for an inline keyboard.
  } deriving (Show, Generic)

instance ToJSON EditMessageCaptionRequest where
  toJSON = toJsonDrop 4

instance FromJSON EditMessageCaptionRequest where
  parseJSON = parseJsonDrop 4

editMessageCaptionRequest :: Text -> Int -> Maybe Text -> EditMessageCaptionRequest
editMessageCaptionRequest chatId messageId caption = EditMessageCaptionRequest (Just chatId) (Just messageId) Nothing caption Nothing

editInlineMessageCaptionRequest :: Text -> Maybe Text -> EditMessageCaptionRequest
editInlineMessageCaptionRequest inlineMessageId caption = EditMessageCaptionRequest Nothing Nothing (Just inlineMessageId) caption Nothing

data EditMessageReplyMarkupRequest = EditMessageReplyMarkupRequest
  {
    emrm_chat_id :: Maybe Text -- ^ Required if `inline_message_id` is not specified. Unique identifier for the target chat or username of the target channel (in the format `@channelusername`)
  , emrm_message_id :: Maybe Int -- ^ Required if `inline_message_id` is not specified. Unique identifier of the sent message
  , emrm_inline_message_id :: Maybe Text -- ^ Required if `chat_id` and `message_id` are not specified. Identifier of the inline message
  , emrm_reply_markup :: Maybe InlineKeyboardMarkup -- ^ A JSON-serialized object for an inline keyboard.
  } deriving (Show, Generic)

instance ToJSON EditMessageReplyMarkupRequest where
  toJSON = toJsonDrop 5

instance FromJSON EditMessageReplyMarkupRequest where
  parseJSON = parseJsonDrop 5

editMessageReplyMarkupRequest :: Text -> Int -> Maybe InlineKeyboardMarkup -> EditMessageReplyMarkupRequest
editMessageReplyMarkupRequest chatId messageId keyboard = EditMessageReplyMarkupRequest (Just chatId) (Just messageId) Nothing keyboard

editInlineMessageReplyMarkupRequest :: Text -> Maybe InlineKeyboardMarkup -> EditMessageReplyMarkupRequest
editInlineMessageReplyMarkupRequest inlineMessageId keyboard = EditMessageReplyMarkupRequest Nothing Nothing (Just inlineMessageId) keyboard
