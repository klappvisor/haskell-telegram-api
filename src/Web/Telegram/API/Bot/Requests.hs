{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts  #-}

-- | This module contains data objects which represents requests to Telegram Bot API
module Web.Telegram.API.Bot.Requests
    ( -- * Types
      SendMessageRequest             (..)
    , ForwardMessageRequest          (..)
    , SendPhotoRequest               (..)
    , SendAudioRequest               (..)
    , SendDocumentRequest            (..)
    , SendStickerRequest             (..)
    , SendVideoRequest               (..)
    , SendVoiceRequest               (..)
    , SendLocationRequest            (..)
    , SendChatActionRequest          (..)
    , ChatAction                     (..)
    , AnswerInlineQueryRequest       (..)
    , AnswerCallbackQueryRequest     (..)
    , ReplyKeyboard                  (..)
    , EditMessageTextRequest         (..)
    , EditMessageCaptionRequest      (..)
    , EditMessageReplyMarkupRequest  (..)
    ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           GHC.TypeLits
import           Web.Telegram.API.Bot.JsonExt
import           Web.Telegram.API.Bot.Data

-- | This object represents request for 'sendMessage'
data SendMessageRequest = SendMessageRequest
  {
    message_chat_id                  :: Text -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , message_text                     :: Text -- ^ Text of the message to be sent
  , message_parse_mode               :: Maybe ParseMode -- ^ Send 'Markdown', if you want Telegram apps to show bold, italic and inline URLs in your bot's message
  , message_disable_web_page_preview :: Maybe Bool -- ^ Disables link previews for links in this message
  , message_reply_to_message_id      :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , message_reply_markup             :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Show, Generic)

instance ToJSON SendMessageRequest where
  toJSON = toJsonDrop 8

instance FromJSON SendMessageRequest where
  parseJSON = parseJsonDrop 8

-- | This object represents request for 'forwardMessage'
data ForwardMessageRequest = ForwardMessageRequest
  {
    forward_chat_id :: Text -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , forward_from_chat_id :: Text -- ^ Unique identifier for the chat where the original message was sent (or channel username in the format @@channelusername@)
  , forward_mesage_id :: Int -- ^ Unique message identifier
  } deriving (Show, Generic)

instance ToJSON ForwardMessageRequest where
  toJSON = toJsonDrop 8

instance FromJSON ForwardMessageRequest where
  parseJSON = parseJsonDrop 8

-- | This object represents request for 'sendPhoto'
data SendPhotoRequest = SendPhotoRequest
  {
    photo_chat_id             :: Text -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , photo_photo               :: Text -- ^ Photo to send. Pass a file_id as String to resend a photo that is already on the Telegram servers
  , photo_caption             :: Maybe Text -- ^ Photo caption (may also be used when resending photos by file_id), 0-200 characters.
  , photo_reply_to_message_id :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , photo_reply_markup        :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Show, Generic)

instance ToJSON SendPhotoRequest where
  toJSON = toJsonDrop 6

instance FromJSON SendPhotoRequest where
  parseJSON = parseJsonDrop 6

-- | This object represents request for 'sendAudio'
data SendAudioRequest = SendAudioRequest
  {
    _audio_chat_id             :: Text -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , _audio_audio               :: Text -- ^ Audio file to send. Pass a file_id as String to resend an audio that is already on the Telegram servers.
  , _audio_duration            :: Maybe Int -- ^ Duration of the audio in seconds
  , _audio_performer           :: Maybe Text -- ^ Performer
  , _audio_title               :: Maybe Text -- ^ Track name
  , _audio_reply_to_message_id :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , _audio_reply_markup        :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Show, Generic)

instance ToJSON SendAudioRequest where
  toJSON = toJsonDrop 7

instance FromJSON SendAudioRequest where
  parseJSON = parseJsonDrop 7

-- | This object represents request for 'sendSticker'
data SendStickerRequest = SendStickerRequest
  {
    sticker_chat_id                  :: Text -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , sticker_sticker                  :: Text -- ^ Sticker to send. A file_id as String to resend a sticker that is already on the Telegram servers
  , sticker_reply_to_message_id      :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , sticker_reply_markup             :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Show, Generic)

instance ToJSON SendStickerRequest where
  toJSON = toJsonDrop 8

instance FromJSON SendStickerRequest where
  parseJSON = parseJsonDrop 8

-- | This object represents request for 'sendDocument'
data SendDocumentRequest = SendDocumentRequest
  {
    document_chat_id                  :: Text -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , document_document                 :: Text -- ^ File to send. A file_id as String to resend a file that is already on the Telegram servers
  , document_reply_to_message_id      :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , document_reply_markup             :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Show, Generic)

instance ToJSON SendDocumentRequest where
  toJSON = toJsonDrop 9

instance FromJSON SendDocumentRequest where
  parseJSON = parseJsonDrop 9

-- | This object represents request for 'sendVideo'
data SendVideoRequest = SendVideoRequest
  {
    _video_chat_id                  :: Text -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , _video_video                    :: Text -- ^ Video to send. A file_id as String to resend a video that is already on the Telegram servers
  , _video_duration                 :: Maybe Int -- ^ Duration of sent video in seconds
  , _video_caption                  :: Maybe Text -- ^ Video caption, 0-200 characters.
  , _video_reply_to_message_id      :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , _video_reply_markup             :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Show, Generic)

instance ToJSON SendVideoRequest where
  toJSON = toJsonDrop 7

instance FromJSON SendVideoRequest where
  parseJSON = parseJsonDrop 7

-- | This object represents request for 'sendVoice'
data SendVoiceRequest = SendVoiceRequest
  {
    _voice_chat_id                  :: Text -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , _voice_voice                    :: Text -- ^ Audio file to send. A file_id as String to resend an audio that is already on the Telegram servers
  , _voice_duration                 :: Maybe Int -- ^ Duration of sent audio in seconds
  , _voice_reply_to_message_id      :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , _voice_reply_markup             :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Show, Generic)

instance ToJSON SendVoiceRequest where
  toJSON = toJsonDrop 7

instance FromJSON SendVoiceRequest where
  parseJSON = parseJsonDrop 7

-- | This object represents request for 'sendLocation'
data SendLocationRequest = SendLocationRequest
  {
    location_chat_id                :: Text -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , location_latitude               :: Float -- ^ Latitude of location
  , location_longitude              :: Float -- ^ Longitude of location
  , location_reply_to_message_id    :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , location_reply_markup           :: Maybe ReplyKeyboard -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } deriving (Show, Generic)

instance ToJSON SendLocationRequest where
  toJSON = toJsonDrop 9

instance FromJSON SendLocationRequest where
  parseJSON = parseJsonDrop 9

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


data AnswerInlineQueryRequest = AnswerInlineQueryRequest
  {
    query_inline_query_id :: Text -- ^ Unique identifier for the answered query
  , query_results         :: [InlineQueryResult] -- ^ A JSON-serialized array of results for the inline query
  , query_cache_time      :: Maybe Int -- ^ The maximum amount of time in seconds that the result of the inline query may be cached on the server. Defaults to 300.
  , query_is_personal     :: Maybe Bool -- ^ Pass True, if results may be cached on the server side only for the user that sent the query. By default, results may be returned to any user who sends the same query
  , query_next_offset     :: Maybe Text -- ^ Pass the offset that a client should send in the next query with the same text to receive more results. Pass an empty string if there are no more results or if you don‘t support pagination. Offset length can’t exceed 64 bytes.
  } deriving (Show, Generic)

instance ToJSON AnswerInlineQueryRequest where
  toJSON = toJsonDrop 6

instance FromJSON AnswerInlineQueryRequest where
  parseJSON = parseJsonDrop 6

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

data ReplyKeyboard =
  -- | This object represents a custom keyboard with reply options
  ReplyKeyboardMarkup
  {
    reply_keyboard             :: [[Text]] -- ^ Array of button rows, each represented by an Array of Strings
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

data EditMessageTextRequest = EditMessageTextRequest
  {
    emt_chat_id :: Maybe Text -- ^ Required if `inline_message_id` is not specified. Unique identifier for the target chat or username of the target channel (in the format `@channelusername`)
  , emt_message_id :: Maybe Integer -- ^ if `inline_message_id` is not specified. Unique identifier of the sent message
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

data EditMessageCaptionRequest = EditMessageCaptionRequest
  {
    emc_chat_id :: Maybe Text -- ^ Required if `inline_message_id` is not specified. Unique identifier for the target chat or username of the target channel (in the format `@channelusername`)
  , emc_message_id :: Maybe Integer -- ^ Required if `inline_message_id` is not specified. Unique identifier of the sent message
  , emc_inline_message_id :: Maybe Text -- ^ Required if `chat_id` and `message_id` are not specified. Identifier of the inline message
  , emc_caption :: Maybe Text -- ^ New caption of the message
  , emc_reply_markup :: Maybe InlineKeyboardMarkup -- ^ A JSON-serialized object for an inline keyboard.
  } deriving (Show, Generic)

instance ToJSON EditMessageCaptionRequest where
  toJSON = toJsonDrop 4

instance FromJSON EditMessageCaptionRequest where
  parseJSON = parseJsonDrop 4

data EditMessageReplyMarkupRequest = EditMessageReplyMarkupRequest
  {
    emrm_chat_id :: Maybe Text -- ^ Required if `inline_message_id` is not specified. Unique identifier for the target chat or username of the target channel (in the format `@channelusername`)
  , emrm_message_id :: Maybe Integer -- ^ Required if `inline_message_id` is not specified. Unique identifier of the sent message
  , emrm_inline_message_id :: Maybe Text -- ^ Required if `chat_id` and `message_id` are not specified. Identifier of the inline message
  , emrm_reply_markup :: Maybe InlineKeyboardMarkup -- ^ A JSON-serialized object for an inline keyboard.
  } deriving (Show, Generic)

instance ToJSON EditMessageReplyMarkupRequest where
  toJSON = toJsonDrop 5

instance FromJSON EditMessageReplyMarkupRequest where
  parseJSON = parseJsonDrop 5