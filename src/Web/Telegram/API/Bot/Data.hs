{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | This module contains objects which represent data of Telegram Bot API responses
module Web.Telegram.API.Bot.Data
    ( -- * Types
      User                          (..)
    , Chat                          (..)
    , Message                       (..)
    , PhotoSize                     (..)
    , Audio                         (..)
    , Document                      (..)
    , Sticker                       (..)
    , Video                         (..)
    , Voice                         (..)
    , Contact                       (..)
    , Location                      (..)
    , Update                        (..)
    , File                          (..)
    , UserProfilePhotos             (..)
    , InlineQuery                   (..)
    , ChosenInlineResult            (..)
    , InlineQueryResult             (..)
    , InlineKeyboardMarkup          (..)
    , InlineKeyboardButton          (..)
    , CallbackQuery                 (..)
    , ChatType                      (..)
    , ParseMode                     (..)
    ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as Char
import           GHC.Generics
import           GHC.TypeLits
import           Data.List
import           Web.Telegram.API.Bot.JsonExt

-- | This object represents a Telegram user or bot.
data User = User
  {
    user_id :: Int                -- ^ Unique identifier for this user or bot
  , user_first_name :: Text       -- ^ User‘s or bot’s first name
  , user_last_name :: Maybe Text  -- ^ User‘s or bot’s last name
  , user_username :: Maybe Text   -- ^ User‘s or bot’s username
  } deriving (Show, Generic)

instance ToJSON User where
  toJSON = toJsonDrop 5

instance FromJSON User where
  parseJSON = parseJsonDrop 5

-- | This object represents a phone contact.
data Contact = Contact
  {
    contact_phone_number :: Text       -- ^ Contact's phone number
  , contact_first_name   :: Text       -- ^ Contact's first name
  , contact_last_name    :: Maybe Text -- ^ Contact's last name
  , contact_user_id      :: Maybe Int  -- ^ Contact's user identifier in Telegram
  } deriving (Show, Generic)

instance ToJSON Contact where
  toJSON = toJsonDrop 8

instance FromJSON Contact where
  parseJSON = parseJsonDrop 8

-- | This object represents a chat.
data Chat = Chat
  {
    chat_id :: Int                -- ^ Unique identifier for this chat, not exceeding 1e13 by absolute value
  , chat_type :: ChatType         -- ^ Type of chat, can be either 'Private', 'Group', 'Supergroup' or 'Channel'
  , chat_title :: Maybe Text      -- ^ Title, for channels and group chats
  , chat_username :: Maybe Text   -- ^ Username, for private chats and channels if available
  , chat_first_name :: Maybe Text -- ^ First name of the other party in a private chat
  , chat_last_name :: Maybe Text  -- ^ Last name of the other party in a private chat
  } deriving (Show, Generic)

instance ToJSON Chat where
  toJSON = toJsonDrop 5

instance FromJSON Chat where
  parseJSON = parseJsonDrop 5

-- | Type of chat.
data ChatType = Private
              | Group
              | Supergroup
              | Channel deriving (Show, Generic)

instance ToJSON ChatType where
  toJSON Private        = "private"
  toJSON Group          = "group"
  toJSON Supergroup     = "supergroup"
  toJSON Channel        = "channel"

instance FromJSON ChatType where
  parseJSON "private"    = pure Private
  parseJSON "group"      = pure Group
  parseJSON "supergroup" = pure Supergroup
  parseJSON "channel"    = pure Channel
  parseJSON _            = fail "Failed to parse ChatType"

-- | Parse mode for text message
data ParseMode = Markdown | HTML deriving (Show, Generic)

instance ToJSON ParseMode where
  toJSON Markdown = "Markdown"
  toJSON HTML = "HTML"

instance FromJSON ParseMode where
  parseJSON "Markdown" = pure $ Markdown
  parseJSON "HTML" = pure $ HTML
  parseJSON _          = fail "Failed to parse ParseMode"

-- | This object represents one size of a photo or a 'File' / 'Sticker' thumbnail.
data PhotoSize = PhotoSize
  {
    photo_file_id   :: Text       -- ^ Unique identifier for this file
  , photo_width     :: Int        -- ^ Photo width
  , photo_height    :: Int        -- ^ Photo height
  , photo_file_size :: Maybe Int  -- ^ File size
  } deriving (Show, Generic)

instance ToJSON PhotoSize where
  toJSON = toJsonDrop 6

instance FromJSON PhotoSize where
  parseJSON = parseJsonDrop 6

-- | This object represents an audio file to be treated as music by the Telegram clients.
data Audio = Audio
  {
    audio_file_id   :: Text       -- ^ Unique identifier for this file
  , audio_duration  :: Int        -- ^ Duration of the audio in seconds as defined by sender
  , audio_performer :: Maybe Text -- ^ Performer of the audio as defined by sender or by audio tags
  , audio_title     :: Maybe Text -- ^ Title of the audio as defined by sender or by audio tags
  , audio_mime_type :: Maybe Text -- ^ MIME type of the file as defined by sender
  , audio_file_size :: Maybe Int  -- ^ File size
  } deriving (Show, Generic)

instance ToJSON Audio where
  toJSON = toJsonDrop 6

instance FromJSON Audio where
  parseJSON = parseJsonDrop 6

-- | This object represents a general file (as opposed to 'PhotoSize', 'Voice' messages and 'Audio' files).
data Document = Document
  {
    doc_file_id   :: Text             -- ^ Unique file identifier
  , doc_thumb     :: Maybe PhotoSize  -- ^ Document thumbnail as defined by sender
  , doc_file_name :: Maybe Text       -- ^ Original filename as defined by sender
  , doc_mime_type :: Maybe Text       -- ^ MIME type of the file as defined by sender
  , doc_file_size :: Maybe Int        -- ^ File size
  } deriving (Show, Generic)

instance ToJSON Document where
  toJSON = toJsonDrop 4

instance FromJSON Document where
  parseJSON = parseJsonDrop 4

-- | This object represents a sticker.
data Sticker = Sticker
  {
    sticker_file_id   :: Text             -- ^ Unique identifier for this file
  , sticker_width     :: Int              -- ^ Sticker width
  , sticker_height    :: Int              -- ^ Sticker height
  , sticker_thumb     :: Maybe PhotoSize  -- ^ Sticker thumbnail in .webp or .jpg format
  , sticker_file_size :: Maybe Int        -- ^ File size
  } deriving (Show, Generic)

instance ToJSON Sticker where
  toJSON = toJsonDrop 8

instance FromJSON Sticker where
  parseJSON = parseJsonDrop 8

-- | This object represents a video file.
data Video = Video
  {
    video_file_id   :: Text             -- ^ Unique identifier for this file
  , video_width     :: Int              -- ^ Video width as defined by sender
  , video_height    :: Int              -- ^ Video height as defined by sender
  , video_duration  :: Int              -- ^ Duration of the video in seconds as defined by sender
  , video_thumb     :: Maybe PhotoSize  -- ^ Video thumbnail
  , video_mime_type :: Maybe Text       -- ^ MIME type of a file as defined by sender
  , video_file_size :: Maybe Int        -- ^ File size
  } deriving (Show, Generic)

instance ToJSON Video where
  toJSON = toJsonDrop 6

instance FromJSON Video where
  parseJSON = parseJsonDrop 6

-- | This object represents a voice note.
data Voice = Voice
  {
    voice_file_id   :: Text       -- ^ Unique identifier for this file
  , voice_duration  :: Int        -- ^ Duration of the audio in seconds as defined by sender
  , voice_mime_type :: Maybe Text -- ^ MIME type of the file as defined by sender
  , voice_file_size :: Maybe Int  -- ^ File size
  } deriving (Show, Generic)

instance ToJSON Voice where
  toJSON = toJsonDrop 6

instance FromJSON Voice where
  parseJSON = parseJsonDrop 6

-- | This object represents an incoming inline query. When the user sends an empty query, your bot could return some default or trending results.
data InlineQuery = InlineQuery
  {
    query_id        :: Text -- ^ Unique identifier for this query
  , query_from      :: User -- ^ Sender
  , query_query     :: Text -- ^ Text of the query
  , query_offset    :: Text -- ^ Offset of the results to be returned, can be controlled by the bot
  } deriving (Show, Generic)

instance ToJSON InlineQuery where
  toJSON = toJsonDrop 6

instance FromJSON InlineQuery where
  parseJSON = parseJsonDrop 6

-- | This object represents a result of an inline query that was chosen by the user and sent to their chat partner.
data ChosenInlineResult = ChosenInlineResult
  {
    chosen_result_id :: Text -- ^ Unique identifier for this query
  , chosen_from      :: User -- ^ Sender
  , chosen_query     :: Text -- ^ Text of the query
  } deriving (Show, Generic)

instance ToJSON ChosenInlineResult where
  toJSON = toJsonDrop 7

instance FromJSON ChosenInlineResult where
  parseJSON = parseJsonDrop 7

data InlineQueryResult =
  -- | Represents a link to an article or web page.
  InlineQueryResultArticle
  {
    iq_res_id                              :: Text -- ^ Unique identifier for this result, 1-64 Bytes
  , iq_res_title                           :: Maybe Text -- ^ Title of the result
  , iq_res_reply_markup                    :: Maybe InlineKeyboardMarkup
  , iq_res_message_text                    :: Maybe Text -- ^ Text of the message to be sent
  , iq_res_parse_mode                      :: Maybe ParseMode -- Send 'Markdown', if you want Telegram apps to show bold, italic and inline URLs in your bot's message.
  , iq_res_disable_web_page_preview        :: Maybe Bool -- ^ Disables link previews for links in the sent message
  , iq_res_url                             :: Maybe Text -- ^ URL of the result
  , iq_res_hide_url                        :: Maybe Bool -- ^ Pass True, if you don't want the URL to be shown in the message
  , iq_res_description                     :: Maybe Text -- ^ Short description of the result
  , iq_res_thumb_url                       :: Maybe Text -- ^ Url of the thumbnail for the result
  , iq_res_thumb_width                     :: Maybe Int -- ^ Thumbnail width
  , iq_res_thumb_height                    :: Maybe Int -- ^ Thumbnail height
  }
  -- | Represents a link to a photo. By default, this photo will be sent by the user with optional caption. Alternatively, you can provide message_text to send it instead of photo.
  | InlineQueryResultPhoto
  {
    iq_res_id                              :: Text -- ^ Unique identifier for this result, 1-64 bytes
  , iq_res_photo_url                       :: Text -- ^ A valid URL of the photo. Photo must be in jpeg format. Photo size must not exceed 5MB
  , iq_res_photo_width                     :: Maybe Int -- ^ Optional. Width of the photo
  , iq_res_photo_height                    :: Maybe Int -- ^ Optional. Height of the photo
  , iq_res_thumb_url                       :: Maybe Text -- ^ URL of the thumbnail for the photo
  , iq_res_title                           :: Maybe Text -- ^ Title for the result
  , iq_res_description                     :: Maybe Text -- ^ Short description of the result
  , iq_res_caption                         :: Maybe Text -- ^ Caption of the photo to be sent, 0-200 characters
  , iq_res_message_text                    :: Maybe Text -- ^ Text of a message to be sent instead of the photo, 1-512 characters
  , iq_res_parse_mode                      :: Maybe ParseMode -- ^ Send 'Markdown', if you want Telegram apps to show bold, italic and inline URLs in your bot's message.
  , iq_res_disable_web_page_preview        :: Maybe Bool -- ^ Disables link previews for links in the sent message
  }
  -- | Represents a link to an animated GIF file. By default, this animated GIF file will be sent by the user with optional caption. Alternatively, you can provide message_text to send it instead of the animation.
  | InlineQueryResultGif
  {
    iq_res_id                              :: Text -- ^ Unique identifier for this result, 1-64 bytes
  , iq_res_gif_url                         :: Text -- ^ A valid URL for the GIF file. File size must not exceed 1MB
  , iq_res_gif_width                       :: Maybe Int -- ^ Width of the GIF
  , iq_res_gif_height                      :: Maybe Int -- ^ Height of the GIF
  , iq_res_thumb_url                       :: Maybe Text -- ^ URL of the static thumbnail for the result (jpeg or gif)
  , iq_res_title                           :: Maybe Text -- ^ Title for the result
  , iq_res_caption                         :: Maybe Text -- ^ Caption of the GIF file to be sent, 0-200 characters
  , iq_res_message_text                    :: Maybe Text -- ^ Text of a message to be sent instead of the animation, 1-512 characters
  , iq_res_parse_mode                      :: Maybe ParseMode -- ^ Send 'Markdown', if you want Telegram apps to show bold, italic and inline URLs in your bot's message.
  , iq_res_disable_web_page_preview        :: Maybe Bool -- ^ Disables link previews for links in the sent message
  }
  -- | Represents a link to a video animation (H.264/MPEG-4 AVC video without sound). By default, this animated MPEG-4 file will be sent by the user with optional caption. Alternatively, you can provide message_text to send it instead of the animation.
  | InlineQueryResultMpeg4Gif
  {
    iq_res_id                              :: Text -- ^ Unique identifier for this result, 1-64 bytes
  , iq_res_mpeg4_url                       :: Text -- ^ A valid URL for the MP4 file. File size must not exceed 1MB
  , iq_res_mpeg4_width                     :: Maybe Int -- ^ Video width
  , iq_res_mpeg4_height                    :: Maybe Int -- ^ Video height
  , iq_res_thumb_url                       :: Maybe Text -- ^ URL of the static thumbnail (jpeg or gif) for the result
  , iq_res_title                           :: Maybe Text -- ^ Title for the result
  , iq_res_caption                         :: Maybe Text -- ^ Caption of the MPEG-4 file to be sent, 0-200 characters
  , iq_res_message_text                    :: Maybe Text -- ^ Text of a message to be sent instead of the animation, 1-512 characters
  , iq_res_parse_mode                      :: Maybe ParseMode -- ^ Send 'Markdown', if you want Telegram apps to show bold, italic and inline URLs in your bot's message.
  , iq_res_disable_web_page_preview        :: Maybe Bool -- ^ Disables link previews for links in the sent message
  }
  -- | Represents link to a page containing an embedded video player or a video file.
  | InlineQueryResultVideo
  {
    iq_res_id                              :: Text -- ^ Unique identifier for this result, 1-64 bytes
  , iq_res_video_url                       :: Text -- ^ A valid URL for the embedded video player or video file
  , iq_res_mime_type                       :: Text -- ^ Mime type of the content of video url, “text/html” or “video/mp4”
  , iq_res_message_text                    :: Maybe Text -- ^ Text of the message to be sent with the video, 1-512 characters
  , iq_res_parse_mode                      :: Maybe ParseMode -- ^ Send 'Markdown', if you want Telegram apps to show bold, italic and inline URLs in your bot's message.
  , iq_res_disable_web_page_preview        :: Maybe Bool -- ^ Disables link previews for links in the sent message
  , iq_res_video_width                     :: Maybe Int -- ^ Video width
  , iq_res_video_height                    :: Maybe Int -- ^ Video height
  , iq_res_video_duration                  :: Maybe Int -- ^ Video duration in seconds
  , iq_res_thumb_url                       :: Maybe Text -- ^ URL of the thumbnail (jpeg only) for the video
  , iq_res_title                           :: Maybe Text -- ^ Title for the result
  , iq_res_description                     :: Maybe Text -- ^ Short description of the result
  } deriving (Show, Generic)

tagModifier "InlineQueryResultMpeg4Gif" = "mpeg4_gif"
tagModifier x = ((drop 17) . (fmap (Char.toLower))) x

inlineQueryJSONOptions :: Options
inlineQueryJSONOptions = defaultOptions {
    fieldLabelModifier     = drop 7
  , omitNothingFields      = True
  , sumEncoding            = TaggedObject { tagFieldName = "type", contentsFieldName = undefined }
  , constructorTagModifier = tagModifier
  }

instance ToJSON InlineQueryResult where
  toJSON = genericToJSON inlineQueryJSONOptions

instance FromJSON InlineQueryResult where
  parseJSON = genericParseJSON inlineQueryJSONOptions

data InlineKeyboardMarkup = InlineKeyboardMarkup
  {
    inline_keyboard :: [[InlineKeyboardButton]]
  } deriving (FromJSON, ToJSON, Show, Generic)

data InlineKeyboardButton = InlineKeyboardButton
  {
    iq_kb_text :: Text
  , iq_kb_url :: Maybe Text
  , iq_kb_callback_data :: Maybe Text
  , iq_kb_switch_inline_query :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON InlineKeyboardButton where
  toJSON = toJsonDrop 6

instance FromJSON InlineKeyboardButton where
  parseJSON = parseJsonDrop 6

data CallbackQuery = CallbackQuery
  {
    cq_id :: Text
  , cq_from :: User
  , cq_message :: Maybe Message
  , cq_inline_message_id :: Maybe Text
  , cq_data :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON CallbackQuery where
  toJSON = toJsonDrop 3

instance FromJSON CallbackQuery where
  parseJSON = parseJsonDrop 3

-- | This object represents an incoming update.
-- Only one of the optional parameters can be present in any given update.
data Update = Update
  {
    update_id            :: Int   -- ^ The update's unique identifier. Update identifiers start from a certain positive number and increase sequentially. This ID becomes especially handy if you’re using 'setWebhooks', since it allows you to ignore repeated updates or to restore the correct update sequence, should they get out of order.
  , message              :: Maybe Message -- ^ New incoming message of any kind — text, photo, sticker, etc.
  , inline_query         :: Maybe InlineQuery -- ^ New incoming inline query
  , chosen_inline_result :: Maybe ChosenInlineResult -- ^ The result of a inline query that was chosen by a user and sent to their chat partner
  , callback_query       :: Maybe CallbackQuery -- ^ This object represents an incoming callback query from a callback button in an inline keyboard. If the button that originated the query was attached to a message sent by the bot, the field message will be presented. If the button was attached to a message sent via the bot (in inline mode), the field inline_message_id will be presented.
  } deriving (FromJSON, ToJSON, Show, Generic)

-- | This object represents a point on the map.
data Location = Location
  {
    longitude :: Float -- ^ Longitude as defined by sender
  , latitude  :: Float -- ^ Latitude as defined by sender
  } deriving (FromJSON, ToJSON, Show, Generic)

-- | This object represents a file ready to be downloaded. The file can be downloaded via the link
--   @https://api.telegram.org/file/bot<token>/<file_path>@. It is guaranteed that the link will be valid
--   for at least 1 hour. When the link expires, a new one can be requested by calling 'getFile'.
--
--       Maximum file size to download is 20 MB
data File = File
  {
    file_id :: Text         -- ^ Unique identifier for this file
  , file_size :: Maybe Int  -- ^ File size, if known
  , file_path :: Maybe Text -- ^ File path. Use @https://api.telegram.org/file/bot<token>/<file_path>@ to get the file.
  } deriving (FromJSON, ToJSON, Show, Generic)

-- | This object represent a user's profile pictures.
data UserProfilePhotos = UserProfilePhotos
  {
    total_count :: Int      -- ^ Total number of profile pictures the target user has
  , photos :: [[PhotoSize]] -- ^ Requested profile pictures (in up to 4 sizes each)
  }  deriving (FromJSON, ToJSON, Show, Generic)

-- | This object represents a message.
data Message = Message
  {
    message_id :: Int                     -- ^ Unique message identifier
  , from :: Maybe User                        -- ^ Sender, can be empty for messages sent to channels
  , date :: Int                           -- ^ Date the message was sent in Unix time
  , chat :: Chat                          -- ^ Conversation the message belongs to
  , forward_from :: Maybe User            -- ^ For forwarded messages, sender of the original message
  , forward_date :: Maybe Int             -- ^ For forwarded messages, date the original message was sent in Unix time
  , reply_to_message :: Maybe Message     -- ^ For replies, the original message. Note that the 'Message' object in this field will not contain further 'reply_to_message' fields even if it itself is a reply.
  , text :: Maybe Text                    -- ^ For text messages, the actual UTF-8 text of the message
  , audio :: Maybe Audio                  -- ^ Message is an audio file, information about the file
  , document :: Maybe Document            -- ^ Message is a general file, information about the file
  , photo :: Maybe [PhotoSize]            -- ^ Message is a photo, available sizes of the photo
  , sticker :: Maybe Sticker              -- ^ Message is a sticker, information about the sticker
  , video :: Maybe Video                  -- ^ Message is a video, information about the video
  , voice :: Maybe Voice                  -- ^ Message is a voice message, information about the file
  , caption :: Maybe Text                 -- ^ Caption for the photo or video
  , contact :: Maybe Contact              -- ^ Message is a shared contact, information about the contact
  , location :: Maybe Location            -- ^ Message is a shared location, information about the location
  , new_chat_member :: Maybe User    -- ^ A new member was added to the group, information about them (this member may be the bot itself)
  , left_chat_member :: Maybe User   -- ^ A member was removed from the group, information about them (this member may be the bot itself)
  , new_chat_title :: Maybe Text          -- ^ A chat title was changed to this value
  , new_chat_photo :: Maybe [PhotoSize]   -- ^ A chat photo was change to this value
  , delete_chat_photo :: Maybe Bool       -- ^ Service message: the chat photo was deleted
  , group_chat_created :: Maybe Bool      -- ^ Service message: the group has been created
  , supergroup_chat_created :: Maybe Bool -- ^ Service message: the supergroup has been created
  , channel_chat_created :: Maybe Bool    -- ^ Service message: the channel has been created
  , migrate_to_chat_id :: Maybe Int       -- ^ The group has been migrated to a supergroup with the specified identifier, not exceeding 1e13 by absolute value
  , migrate_from_chat_id :: Maybe Int     -- ^ The supergroup has been migrated from a group with the specified identifier, not exceeding 1e13 by absolute value
  } deriving (FromJSON, ToJSON, Show, Generic)
