{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains objects which represent data of Telegram Bot API responses
module Web.Telegram.API.Bot.Data
    ( -- * Types
      User                          (..)
    , ChatMember                    (..)
    , Chat                          (..)
    , Message                       (..)
    , MessageEntity                 (..)
    , PhotoSize                     (..)
    , Audio                         (..)
    , Document                      (..)
    , Game                          (..)
    , Animation                     (..)
    , Sticker                       (..)
    , Video                         (..)
    , Voice                         (..)
    , Venue                         (..)
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
    , CallbackGame                  (..)
    , CallbackQuery                 (..)
    , ChatType                      (..)
    , ParseMode                     (..)
    , InputMessageContent           (..)
    , KeyboardButton                (..)
    , WebhookInfo                   (..)
      -- * Functions
    , inlineKeyboardButton
    , keyboardButton
    , inlineQueryResultArticle
    , inlineQueryResultAudio
    , inlineQueryResultContact
    , inlineQueryResultDocument
    , inlineQueryResultGif
    , inlineQueryResultLocation
    , inlineQueryResultMpeg4Gif
    , inlineQueryResultPhoto
    , inlineQueryResultVenue
    , inlineQueryResultVideo
    , inlineQueryResultVoice
    , inlineQueryResultCachedAudio
    , inlineQueryResultCachedDocument
    , inlineQueryResultCachedGif
    , inlineQueryResultCachedMpeg4Gif
    , inlineQueryResultGame
    , inlineQueryResultCachedPhoto
    , inlineQueryResultCachedSticker
    , inlineQueryResultCachedVideo
    , inlineQueryResultCachedVoice

    ) where

import           Prelude hiding (id)

import           Data.Aeson
import           Data.Maybe()
import           Data.Aeson.Types
import           Data.Text (Text)
import qualified Data.Char as Char
import           GHC.Generics
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
  , chat_all_members_are_administrators :: Maybe Bool -- ^ True if a group has ‘All Members Are Admins’ enabled.
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
  parseJSON "Markdown" = pure Markdown
  parseJSON "HTML" = pure HTML
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

-- | This object represents a game. Use BotFather to create and edit games, their short names will act as unique identifiers.
data Game = Game
  {
    game_title :: Text -- ^ Title of the game
  , game_description :: Text -- ^ Description of the game
  , game_photo :: [PhotoSize] -- ^ Photo that will be displayed in the game message in chats.
  , game_text :: Maybe Text -- ^ Brief description of the game or high scores included in the game message. Can be automatically edited to include current high scores for the game when the bot calls setGameScore, or manually edited using editMessageText. 0-4096 characters.
  , game_text_entities :: Maybe [MessageEntity] -- ^ Special entities that appear in text, such as usernames, URLs, bot commands, etc.
  , game_animation :: Maybe Animation -- ^ Animation that will be displayed in the game message in chats. Upload via BotFather
  } deriving (Show, Generic)

instance ToJSON Game where
  toJSON = toJsonDrop 5

instance FromJSON Game where
  parseJSON = parseJsonDrop 5

-- | This object represents an animation file to be displayed in the message containing a game.
data Animation = Animation
  {
    anim_file_id :: Text -- ^ Unique file identifier
  , anim_thumb :: Maybe PhotoSize -- ^ Animation thumbnail as defined by sender
  , anim_file_name :: Maybe Text -- ^ Original animation filename as defined by sender
  , anim_mime_type :: Maybe Text -- ^ MIME type of the file as defined by sender
  , anim_file_size :: Maybe Int -- ^ File size
  } deriving (Show, Generic)

instance ToJSON Animation where
  toJSON = toJsonDrop 5

instance FromJSON Animation where
  parseJSON = parseJsonDrop 5

-- | This object represents a sticker.
data Sticker = Sticker
  {
    sticker_file_id   :: Text             -- ^ Unique identifier for this file
  , sticker_width     :: Int              -- ^ Sticker width
  , sticker_height    :: Int              -- ^ Sticker height
  , sticker_thumb     :: Maybe PhotoSize  -- ^ Sticker thumbnail in .webp or .jpg format
  , sticker_emoji     :: Maybe Text       -- ^ Emoji associated with the sticker
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
  , query_location  :: Maybe Location -- ^ Sender location, only for bots that request user location
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
    chosen_result_id          :: Text -- ^ The unique identifier for the result that was chosen
  , chosen_from               :: User -- ^ The user that chose the result
  , chosen_location           :: Maybe Location -- ^ Sender location, only for bots that require user location
  , chosen_inline_message_id  :: Maybe Text -- ^ Identifier of the sent inline message. Available only if there is an inline keyboard attached to the message. Will be also received in callback queries and can be used to edit the message.
  , chosen_query              :: Text -- ^ The query that was used to obtain the result
  } deriving (Show, Generic)

instance ToJSON ChosenInlineResult where
  toJSON = toJsonDrop 7

instance FromJSON ChosenInlineResult where
  parseJSON = parseJsonDrop 7

-- | This object represents the content of a message to be sent as a result of an inline query.
data InputMessageContent =
  -- | Represents the content of a text message to be sent as the result of an inline query.
  InputTextMessageContent
  {
    imc_message_text :: Text -- ^ Text of the message to be sent, 1-4096 characters
  , imc_parse_mode :: Maybe ParseMode -- ^ Send 'Markdown' or 'HTML', if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , imc_disable_web_page_preview :: Maybe Bool -- ^ Disables link previews for links in the sent message
  }
  -- | Represents the content of a location message to be sent as the result of an inline query.
  | InputLocationMessageContent
  {
    imc_latitude :: Float -- ^ Latitude of the location in degrees
  , imc_longitude :: Float -- ^ Longitude of the location in degrees
  }
  -- | Represents the content of a venue message to be sent as the result of an inline query.
  | InputVenueMessageContent
  {
    imc_latitude :: Float -- ^ Latitude of the location in degrees
  , imc_longitude :: Float -- ^ Longitude of the location in degrees
  , imc_title :: Text -- ^ Name of the venue
  , imc_address :: Text -- ^ Address of the venue
  , imc_foursquare_id :: Maybe Text -- ^ Foursquare identifier of the venue, if known
  }
  -- | Represents the content of a contact message to be sent as the result of an inline query.
  | InputContactMessageContent
  {
    imc_phone_number :: Text -- ^ Contact's phone number
  , imc_first_name :: Text -- ^ Contact's first name
  , imc_last_name :: Maybe Text -- ^ Contact's last name
  } deriving (Show, Generic)

instance ToJSON InputMessageContent where
  toJSON = toJsonDrop 4

instance FromJSON InputMessageContent where
  parseJSON = parseJsonDrop 4

data InlineQueryResult =
  -- | Represents a link to an article or web page.
  InlineQueryResultArticle
  {
    iq_res_id                              :: Text -- ^ Unique identifier for this result, 1-64 Bytes
  , iq_res_title                           :: Maybe Text -- ^ Title of the result
  , iq_res_input_message_content           :: Maybe InputMessageContent -- ^ Content of the message to be sent
  , iq_res_reply_markup                    :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iq_res_url                             :: Maybe Text -- ^ URL of the result
  , iq_res_hide_url                        :: Maybe Bool -- ^ Pass True, if you don't want the URL to be shown in the message
  , iq_res_description                     :: Maybe Text -- ^ Short description of the result
  , iq_res_thumb_url                       :: Maybe Text -- ^ Url of the thumbnail for the result
  , iq_res_thumb_width                     :: Maybe Int -- ^ Thumbnail width
  , iq_res_thumb_height                    :: Maybe Int -- ^ Thumbnail height
  }
  -- | Represents a link to a photo. By default, this photo will be sent by the user with optional caption. Alternatively, you can use input_message_content to send a message with the specified content instead of the photo.
  | InlineQueryResultPhoto
  {
    iq_res_id                              :: Text -- ^ Unique identifier for this result, 1-64 bytes
  , iq_res_photo_url                       :: Text -- ^ A valid URL of the photo. Photo must be in jpeg format. Photo size must not exceed 5MB
  , iq_res_thumb_url                       :: Maybe Text -- ^ URL of the thumbnail for the photo
  , iq_res_photo_width                     :: Maybe Int -- ^ Optional. Width of the photo
  , iq_res_photo_height                    :: Maybe Int -- ^ Optional. Height of the photo
  , iq_res_title                           :: Maybe Text -- ^ Title for the result
  , iq_res_description                     :: Maybe Text -- ^ Short description of the result
  , iq_res_caption                         :: Maybe Text -- ^ Caption of the photo to be sent, 0-200 characters
  , iq_res_reply_markup                    :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iq_res_input_message_content           :: Maybe InputMessageContent -- ^ Content of the message to be sent instead of the photo
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
  , iq_res_reply_markup                    :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iq_res_input_message_content           :: Maybe InputMessageContent -- ^ Content of the message to be sent instead of the GIF animation
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
  , iq_res_reply_markup                    :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iq_res_input_message_content           :: Maybe InputMessageContent -- ^ Content of the message to be sent instead of the video animation
  }
  -- | Represents link to a page containing an embedded video player or a video file.
  | InlineQueryResultVideo
  {
    iq_res_id                              :: Text -- ^ Unique identifier for this result, 1-64 bytes
  , iq_res_video_url                       :: Text -- ^ A valid URL for the embedded video player or video file
  , iq_res_mime_type                       :: Text -- ^ Mime type of the content of video url, “text/html” or “video/mp4”
  , iq_res_thumb_url                       :: Maybe Text -- ^ URL of the thumbnail (jpeg only) for the video
  , iq_res_title                           :: Maybe Text -- ^ Title for the result
  , iq_res_caption                         :: Maybe Text -- ^ Caption of the video to be sent, 0-200 characters
  , iq_res_video_width                     :: Maybe Int -- ^ Video width
  , iq_res_video_height                    :: Maybe Int -- ^ Video height
  , iq_res_video_duration                  :: Maybe Int -- ^ Video duration in seconds
  , iq_res_description                     :: Maybe Text -- ^ Short description of the result
  , iq_res_reply_markup                    :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iq_res_input_message_content           :: Maybe InputMessageContent -- ^ Content of the message to be sent instead of the video
  }
  -- | Represents a link to an mp3 audio file. By default, this audio file will be sent by the user. Alternatively, you can use input_message_content to send a message with the specified content instead of the audio.
  | InlineQueryResultAudio
  {
    iq_res_id :: Text -- ^ Unique identifier for this result, 1-64 bytes
  , iq_res_audio_url :: Text -- ^ A valid URL for the audio file
  , iq_res_title :: Maybe Text -- ^ Title
  , iq_res_caption :: Maybe Text -- ^ Caption, 0-200 characters
  , iq_res_performer :: Maybe Text -- ^ Performer
  , iq_res_audio_duration :: Maybe Int -- ^ Audio duration in seconds
  , iq_res_reply_markup :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iq_res_input_message_content :: Maybe InputMessageContent -- ^ Content of the message to be sent instead of the audio
  }
  -- | Represents a link to a voice recording in an .ogg container encoded with OPUS. By default, this voice recording will be sent by the user. Alternatively, you can use input_message_content to send a message with the specified content instead of the the voice message.
  | InlineQueryResultVoice
  {
    iq_res_id :: Text -- ^ Unique identifier for this result, 1-64 bytes
  , iq_res_voice_url :: Text -- ^ A valid URL for the voice recording
  , iq_res_title :: Maybe Text -- ^ Recording title
  , iq_res_caption :: Maybe Text -- ^ Caption, 0-200 characters
  , iq_res_voice_duration :: Maybe Int -- ^ Recording duration in seconds
  , iq_res_reply_markup :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iq_res_input_message_content :: Maybe InputMessageContent -- ^ Content of the message to be sent instead of the voice recording
  }
  -- | Represents a link to a file. By default, this file will be sent by the user with an optional caption. Alternatively, you can use input_message_content to send a message with the specified content instead of the file. Currently, only .PDF and .ZIP files can be sent using this method.
  | InlineQueryResultDocument
  {
    iq_res_id :: Text -- ^ Unique identifier for this result, 1-64 bytes
  , iq_res_title :: Maybe Text -- ^ Title for the result
  , iq_res_caption :: Maybe Text -- ^ Caption of the document to be sent, 0-200 characters
  , iq_res_document_url :: Text -- ^ A valid URL for the file
  , iq_res_mime_type :: Text -- ^ Mime type of the content of the file, either “application/pdf” or “application/zip”
  , iq_res_description :: Maybe Text -- ^ Short description of the result
  , iq_res_reply_markup :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iq_res_input_message_content :: Maybe InputMessageContent -- ^ Content of the message to be sent instead of the file
  , iq_res_thumb_url :: Maybe Text -- ^ URL of the thumbnail (jpeg only) for the file
  , iq_res_thumb_width :: Maybe Int -- ^ Thumbnail width
  , iq_res_thumb_height :: Maybe Int -- ^ Thumbnail height
  }
  -- | Represents a location on a map. By default, the location will be sent by the user. Alternatively, you can use input_message_content to send a message with the specified content instead of the location.
  | InlineQueryResultLocation
  {
    iq_res_id :: Text -- ^ Unique identifier for this result, 1-64 Bytes
  , iq_res_latitude :: Float -- ^ Location latitude in degrees
  , iq_res_longitude :: Float -- ^ Location longitude in degrees
  , iq_res_title :: Maybe Text -- ^ Location title
  , iq_res_reply_markup :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iq_res_input_message_content :: Maybe InputMessageContent -- ^ Content of the message to be sent instead of the location
  , iq_res_thumb_url :: Maybe Text -- ^ Url of the thumbnail for the result
  , iq_res_thumb_width :: Maybe Int -- ^ Thumbnail width
  , iq_res_thumb_height :: Maybe Int -- ^ Thumbnail height
  }
  -- | Represents a venue. By default, the venue will be sent by the user. Alternatively, you can use input_message_content to send a message with the specified content instead of the venue.
  | InlineQueryResultVenue
  {
    iq_res_id :: Text -- ^ Unique identifier for this result, 1-64 Bytes
  , iq_res_latitude :: Float -- ^ Latitude of the venue location in degrees
  , iq_res_longitude :: Float -- ^ Longitude of the venue location in degrees
  , iq_res_title :: Maybe Text -- ^ Title of the venue
  , iq_res_address :: Text -- ^ Address of the venue
  , iq_res_foursquare_id :: Maybe Text -- ^ Foursquare identifier of the venue if known
  , iq_res_reply_markup :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iq_res_input_message_content :: Maybe InputMessageContent -- ^ Content of the message to be sent instead of the venue
  , iq_res_thumb_url :: Maybe Text -- ^ Url of the thumbnail for the result
  , iq_res_thumb_width :: Maybe Int -- ^ Thumbnail width
  , iq_res_thumb_height :: Maybe Int -- ^ Thumbnail height
  }
  -- | Represents a link to a photo stored on the Telegram servers. By default, this photo will be sent by the user with an optional caption. Alternatively, you can use input_message_content to send a message with the specified content instead of the photo.
  | InlineQueryResultContact
  {
    iq_res_id :: Text -- ^ Unique identifier for this result, 1-64 Bytes
  , iq_res_phone_number :: Text -- ^ Contact's phone number
  , iq_res_first_name :: Text -- ^ Contact's first name
  , iq_res_last_name :: Maybe Text -- ^ Contact's last name
  , iq_res_reply_markup :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iq_res_input_message_content :: Maybe InputMessageContent -- ^ Content of the message to be sent instead of the contact
  , iq_res_thumb_url :: Maybe Text -- ^ Url of the thumbnail for the result
  , iq_res_thumb_width :: Maybe Int -- ^ Thumbnail width
  , iq_res_thumb_height :: Maybe Int -- ^ Thumbnail height
  }
  -- | Represents a Game.
  | InlineQueryResultGame
  {
    iq_res_id :: Text -- ^ Unique identifier for this result, 1-64 bytes
  , iq_res_game_short_name :: Text -- ^ Short name of the game
  , iq_res_reply_markup :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  }
  -- | Represents a link to a photo stored on the Telegram servers. By default, this photo will be sent by the user with an optional caption. Alternatively, you can use input_message_content to send a message with the specified content instead of the photo.
  | InlineQueryResultCachedPhoto
  {
    iq_res_id :: Text -- ^ Unique identifier for this result, 1-64 bytes
  , iq_res_photo_file_id :: Text -- ^ A valid file identifier of the photo
  , iq_res_title :: Maybe Text -- ^ Title for the result
  , iq_res_description :: Maybe Text -- ^ Short description of the result
  , iq_res_caption :: Maybe Text -- ^ Caption of the photo to be sent, 0-200 characters
  , iq_res_reply_markup :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message
  , iq_res_input_message_content :: Maybe InputMessageContent -- ^ Content of the message to be sent instead of the photo
  }
  -- | Represents a link to an animated GIF file stored on the Telegram servers. By default, this animated GIF file will be sent by the user with an optional caption. Alternatively, you can use input_message_content to send a message with specified content instead of the animation.
  | InlineQueryResultCachedGif
  {
    iq_res_id :: Text -- ^ Unique identifier for this result, 1-64 bytes
  , iq_res_gif_file_id :: Text -- ^ A valid file identifier for the GIF file
  , iq_res_title :: Maybe Text -- ^ Title for the result
  , iq_res_caption :: Maybe Text -- ^ Caption of the GIF file to be sent, 0-200 characters
  , iq_res_reply_markup :: Maybe InlineKeyboardMarkup -- ^ An Inline keyboard attached to the message
  , iq_res_input_message_content :: Maybe InputMessageContent -- ^ Content of the message to be sent instead of the GIF animation
  }
  -- | Represents a link to a video animation (H.264/MPEG-4 AVC video without sound) stored on the Telegram servers. By default, this animated MPEG-4 file will be sent by the user with an optional caption. Alternatively, you can use input_message_content to send a message with the specified content instead of the animation.
  | InlineQueryResultCachedMpeg4Gif
  {
    iq_res_id :: Text -- ^ Unique identifier for this result, 1-64 bytes
  , iq_res_mpeg4_file_id :: Text -- ^ A valid file identifier for the MP4 file
  , iq_res_title :: Maybe Text -- ^ Title for the result
  , iq_res_caption :: Maybe Text -- ^ Caption of the MPEG-4 file to be sent, 0-200 characters
  , iq_res_reply_markup :: Maybe InlineKeyboardMarkup -- ^ An Inline keyboard attached to the message
  , iq_res_input_message_content :: Maybe InputMessageContent -- ^ Content of the message to be sent instead of the video animation
  }
  -- | Represents a link to a sticker stored on the Telegram servers. By default, this sticker will be sent by the user. Alternatively, you can use input_message_content to send a message with the specified content instead of the sticker.
  | InlineQueryResultCachedSticker
  {
    iq_res_id :: Text -- ^ Unique identifier for this result, 1-64 bytes
  , iq_res_sticker_file_id :: Text -- ^ A valid file identifier of the sticker
  , iq_res_reply_markup :: Maybe InlineKeyboardMarkup -- ^ An Inline keyboard attached to the message
  , iq_res_input_message_content :: Maybe InputMessageContent -- ^ Content of the message to be sent instead of the sticker
  }
  -- | Represents a link to a file stored on the Telegram servers. By default, this file will be sent by the user with an optional caption. Alternatively, you can use input_message_content to send a message with the specified content instead of the file. Currently, only pdf-files and zip archives can be sent using this method.
  | InlineQueryResultCachedDocument
  {
    iq_res_id :: Text -- ^ Unique identifier for this result, 1-64 bytes
  , iq_res_title :: Maybe Text -- ^ Title for the result
  , iq_res_document_file_id :: Text -- ^ A valid file identifier for the file
  , iq_res_description :: Maybe Text -- ^ Short description of the result
  , iq_res_caption :: Maybe Text -- ^ Caption of the document to be sent, 0-200 characters
  , iq_res_reply_markup :: Maybe InlineKeyboardMarkup -- ^ An Inline keyboard attached to the message
  , iq_res_input_message_content :: Maybe InputMessageContent -- ^ Content of the message to be sent instead of the file
  }
  -- | Represents a link to a video file stored on the Telegram servers. By default, this video file will be sent by the user with an optional caption. Alternatively, you can use input_message_content to send a message with the specified content instead of the video.
  | InlineQueryResultCachedVideo
  {
    iq_res_id :: Text -- ^ Unique identifier for this result, 1-64 bytes
  , iq_res_video_file_id :: Text -- ^ A valid file identifier for the video file
  , iq_res_title :: Maybe Text -- ^ Title for the result
  , iq_res_description :: Maybe Text -- ^ Short description of the result
  , iq_res_caption :: Maybe Text -- ^ Caption of the video to be sent, 0-200 characters
  , iq_res_reply_markup :: Maybe InlineKeyboardMarkup -- ^ An Inline keyboard attached to the message
  , iq_res_input_message_content :: Maybe InputMessageContent -- ^ Content of the message to be sent instead of the video
  }
  -- | Represents a link to a voice message stored on the Telegram servers. By default, this voice message will be sent by the user. Alternatively, you can use input_message_content to send a message with the specified content instead of the voice message.
  | InlineQueryResultCachedVoice
  {
    iq_res_id :: Text -- ^ Unique identifier for this result, 1-64 bytes
  , iq_res_voice_file_id :: Text -- ^ A valid file identifier for the voice message
  , iq_res_title :: Maybe Text -- ^ Voice message title
  , iq_res_caption :: Maybe Text -- ^ Caption, 0-200 characters
  , iq_res_reply_markup :: Maybe InlineKeyboardMarkup -- ^ An Inline keyboard attached to the message
  , iq_res_input_message_content :: Maybe InputMessageContent -- ^ ontent of the message to be sent instead of the voice message
  }
  -- | Represents a link to an mp3 audio file stored on the Telegram servers. By default, this audio file will be sent by the user. Alternatively, you can use input_message_content to send a message with the specified content instead of the audio.
  | InlineQueryResultCachedAudio
  {
    iq_res_id :: Text -- ^ Unique identifier for this result, 1-64 bytes
  , iq_res_audio_file_id :: Text -- ^ A valid file identifier for the audio file
  , iq_res_caption :: Maybe Text -- ^ Caption, 0-200 characters
  , iq_res_reply_markup :: Maybe InlineKeyboardMarkup -- ^ An Inline keyboard attached to the message
  , iq_res_input_message_content :: Maybe InputMessageContent -- ^ ontent of the message to be sent instead of the audio
  } deriving (Show, Generic)

dropCached :: String -> String
dropCached name = if "Cached" `isPrefixOf` name then drop 6 name else name

tagModifier :: String -> String
tagModifier "InlineQueryResultMpeg4Gif" = "mpeg4_gif"
tagModifier "InlineQueryResultCachedMpeg4Gif" = "mpeg4_gif"
tagModifier x = (fmap Char.toLower . dropCached . drop 17) x

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

inlineQueryResultArticle :: Text -> Text -> InputMessageContent -> InlineQueryResult
inlineQueryResultArticle id title content = InlineQueryResultArticle id (Just title) (Just content) Nothing Nothing Nothing Nothing Nothing Nothing Nothing

inlineQueryResultPhoto :: Text -> Text -> Text -> InlineQueryResult
inlineQueryResultPhoto id photoUrl thumbUlr = InlineQueryResultPhoto id photoUrl (Just thumbUlr) Nothing Nothing Nothing Nothing Nothing Nothing Nothing

inlineQueryResultGif :: Text -> Text -> Text -> InlineQueryResult
inlineQueryResultGif id gifUrl thumbUrl = InlineQueryResultGif id gifUrl Nothing Nothing (Just thumbUrl) Nothing Nothing Nothing Nothing

inlineQueryResultMpeg4Gif :: Text -> Text -> Text -> InlineQueryResult
inlineQueryResultMpeg4Gif id mpeg4Url thumbUrl = InlineQueryResultMpeg4Gif id mpeg4Url Nothing Nothing (Just thumbUrl) Nothing Nothing Nothing Nothing

inlineQueryResultVideo :: Text -> Text -> Text -> Text -> Text -> InlineQueryResult
inlineQueryResultVideo id videoUrl mimeType thumbUrl title = InlineQueryResultVideo id videoUrl mimeType (Just thumbUrl) (Just title) Nothing Nothing Nothing Nothing Nothing Nothing Nothing

inlineQueryResultAudio :: Text -> Text -> Text -> InlineQueryResult
inlineQueryResultAudio id audioUrl title = InlineQueryResultAudio id audioUrl (Just title) Nothing Nothing Nothing Nothing Nothing

inlineQueryResultVoice :: Text -> Text -> Text -> InlineQueryResult
inlineQueryResultVoice id voiceUrl title = InlineQueryResultVoice id voiceUrl (Just title) Nothing Nothing Nothing Nothing

inlineQueryResultDocument :: Text -> Text -> Text -> Text -> InlineQueryResult
inlineQueryResultDocument id title docUrl mimeType = InlineQueryResultDocument id (Just title) Nothing docUrl mimeType Nothing Nothing Nothing Nothing Nothing Nothing

inlineQueryResultLocation :: Text -> Float -> Float -> Text -> InlineQueryResult
inlineQueryResultLocation id lat lon title = InlineQueryResultLocation id lat lon (Just title) Nothing Nothing Nothing Nothing Nothing

inlineQueryResultVenue :: Text -> Float -> Float -> Text -> Text -> InlineQueryResult
inlineQueryResultVenue id lat lon title address = InlineQueryResultVenue id lat lon (Just title) address Nothing Nothing Nothing Nothing Nothing Nothing

inlineQueryResultContact :: Text -> Text -> Text -> InlineQueryResult
inlineQueryResultContact id phoneNumber firstName = InlineQueryResultContact id phoneNumber firstName Nothing Nothing Nothing Nothing Nothing Nothing

inlineQueryResultGame :: Text -> Text -> InlineQueryResult
inlineQueryResultGame id gameShortName = InlineQueryResultGame id gameShortName Nothing

inlineQueryResultCachedPhoto :: Text -> Text -> InlineQueryResult
inlineQueryResultCachedPhoto id fileId = InlineQueryResultCachedPhoto id fileId Nothing Nothing Nothing Nothing Nothing

inlineQueryResultCachedGif :: Text -> Text -> InlineQueryResult
inlineQueryResultCachedGif id fileId = InlineQueryResultCachedGif id fileId Nothing Nothing Nothing Nothing

inlineQueryResultCachedMpeg4Gif :: Text -> Text -> InlineQueryResult
inlineQueryResultCachedMpeg4Gif id fileId = InlineQueryResultCachedMpeg4Gif id fileId Nothing Nothing Nothing Nothing

inlineQueryResultCachedSticker :: Text -> Text -> InlineQueryResult
inlineQueryResultCachedSticker id fileId = InlineQueryResultCachedSticker id fileId Nothing Nothing

inlineQueryResultCachedDocument :: Text -> Text -> Text -> InlineQueryResult
inlineQueryResultCachedDocument id fileId title = InlineQueryResultCachedDocument id (Just title) fileId Nothing Nothing Nothing Nothing

inlineQueryResultCachedVideo :: Text -> Text -> Text -> InlineQueryResult
inlineQueryResultCachedVideo id fileId title = InlineQueryResultCachedVideo id fileId (Just title) Nothing Nothing Nothing Nothing

inlineQueryResultCachedVoice :: Text -> Text -> Text -> InlineQueryResult
inlineQueryResultCachedVoice id fileId title = InlineQueryResultCachedVoice id fileId (Just title) Nothing Nothing Nothing

inlineQueryResultCachedAudio :: Text -> Text -> InlineQueryResult
inlineQueryResultCachedAudio id fileId = InlineQueryResultCachedAudio id fileId Nothing Nothing Nothing

data InlineKeyboardMarkup = InlineKeyboardMarkup
  {
    inline_keyboard :: [[InlineKeyboardButton]]
  } deriving (FromJSON, ToJSON, Show, Generic)

data InlineKeyboardButton = InlineKeyboardButton
  {
    ikb_text :: Text
  , ikb_url :: Maybe Text
  , ikb_callback_data :: Maybe Text
  , ikb_switch_inline_query :: Maybe Text
  , ikb_callback_game :: Maybe CallbackGame
  , ikb_switch_inline_query_current_chat :: Maybe Text -- ^ If set, pressing the button will insert the bot‘s username and the specified inline query in the current chat's input field. Can be empty, in which case only the bot’s username will be inserted.
  } deriving (Show, Generic)

instance ToJSON InlineKeyboardButton where
  toJSON = toJsonDrop 4

instance FromJSON InlineKeyboardButton where
  parseJSON = parseJsonDrop 4

inlineKeyboardButton :: Text -> InlineKeyboardButton
inlineKeyboardButton buttonText =
  InlineKeyboardButton buttonText Nothing Nothing Nothing Nothing Nothing

data CallbackGame = CallbackGame
  {
  } deriving (Show, Generic)

instance ToJSON CallbackGame where
  toJSON = toJsonDrop 3

instance FromJSON CallbackGame where
  parseJSON = parseJsonDrop 3

data CallbackQuery = CallbackQuery
  {
    cq_id :: Text
  , cq_from :: User
  , cq_message :: Maybe Message
  , cq_inline_message_id :: Maybe Text
  , cq_chat_instance :: Text
  , cq_data :: Maybe Text
  , cq_game_short_name :: Maybe Text
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
  , edited_message       :: Maybe Message -- ^ New version of a message that is known to the bot and was edited
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
  } deriving (FromJSON, ToJSON, Show, Generic)

data ChatMember = ChatMember
  {
    cm_user :: User -- ^ Information about the user
  , cm_status :: Text -- ^ The member's status in the chat. Can be “creator”, “administrator”, “member”, “left” or “kicked”
  } deriving (Show, Generic)

instance ToJSON ChatMember where
  toJSON = toJsonDrop 3

instance FromJSON ChatMember where
  parseJSON = parseJsonDrop 3

-- | This object represents a message.
data Message = Message
  {
    message_id :: Int                     -- ^ Unique message identifier
  , from :: Maybe User                    -- ^ Sender, can be empty for messages sent to channels
  , date :: Int                           -- ^ Date the message was sent in Unix time
  , chat :: Chat                          -- ^ Conversation the message belongs to
  , forward_from :: Maybe User            -- ^ For forwarded messages, sender of the original message
  , forward_from_chat :: Maybe Chat       -- ^ For messages forwarded from a channel, information about the original channel
  , forward_date :: Maybe Int             -- ^ For forwarded messages, date the original message was sent in Unix time
  , reply_to_message :: Maybe Message     -- ^ For replies, the original message. Note that the 'Message' object in this field will not contain further 'reply_to_message' fields even if it itself is a reply.
  , edit_date :: Maybe Int                -- ^ Date the message was last edited in Unix time
  , text :: Maybe Text                    -- ^ For text messages, the actual UTF-8 text of the message
  , entities :: Maybe [MessageEntity]     -- ^ For text messages, special entities like usernames, URLs, bot commands, etc. that appear in the text
  , audio :: Maybe Audio                  -- ^ Message is an audio file, information about the file
  , document :: Maybe Document            -- ^ Message is a general file, information about the file
  , game :: Maybe Game                    -- ^ Message is a game, information about the game
  , photo :: Maybe [PhotoSize]            -- ^ Message is a photo, available sizes of the photo
  , sticker :: Maybe Sticker              -- ^ Message is a sticker, information about the sticker
  , video :: Maybe Video                  -- ^ Message is a video, information about the video
  , voice :: Maybe Voice                  -- ^ Message is a voice message, information about the file
  , caption :: Maybe Text                 -- ^ Caption for the photo or video
  , contact :: Maybe Contact              -- ^ Message is a shared contact, information about the contact
  , location :: Maybe Location            -- ^ Message is a shared location, information about the location
  , venue :: Maybe Venue                  -- ^ Message is a venue, information about the venue
  , new_chat_member :: Maybe User         -- ^ A new member was added to the group, information about them (this member may be the bot itself)
  , left_chat_member :: Maybe User        -- ^ A member was removed from the group, information about them (this member may be the bot itself)
  , new_chat_title :: Maybe Text          -- ^ A chat title was changed to this value
  , new_chat_photo :: Maybe [PhotoSize]   -- ^ A chat photo was change to this value
  , delete_chat_photo :: Maybe Bool       -- ^ Service message: the chat photo was deleted
  , group_chat_created :: Maybe Bool      -- ^ Service message: the group has been created
  , supergroup_chat_created :: Maybe Bool -- ^ Service message: the supergroup has been created
  , channel_chat_created :: Maybe Bool    -- ^ Service message: the channel has been created
  , migrate_to_chat_id :: Maybe Int       -- ^ The group has been migrated to a supergroup with the specified identifier, not exceeding 1e13 by absolute value
  , migrate_from_chat_id :: Maybe Int     -- ^ The supergroup has been migrated from a group with the specified identifier, not exceeding 1e13 by absolute value
  , pinned_message :: Maybe Message       -- ^ Specified message was pinned. Note that the Message object in this field will not contain further reply_to_message fields even if it is itself a reply.
  } deriving (FromJSON, ToJSON, Show, Generic)

-- | This object represents one special entity in a text message. For example, hashtags, usernames, URLs, etc.
data MessageEntity = MessageEntity
  {
    me_type :: Text       -- ^ Type of the entity. Can be mention (@username), hashtag, bot_command, url, email, bold (bold text), italic (italic text), code (monowidth string), pre (monowidth block), text_link (for clickable text URLs), text_mention (for users without usernames)
  , me_offset :: Int      -- ^ Offset in UTF-16 code units to the start of the entity
  , me_length :: Int      -- ^ Length of the entity in UTF-16 code units
  , me_url :: Maybe Text  -- ^ For “text_link” only, url that will be opened after user taps on the text
  , me_user :: Maybe User -- ^ For “text_mention” only, the mentioned user
  } deriving (Show, Generic)

instance ToJSON MessageEntity where
  toJSON = toJsonDrop 3

instance FromJSON MessageEntity where
  parseJSON = parseJsonDrop 3

-- | This object represents a venue.
data Venue = Venue
  {
    venue_location :: Location          -- ^ Venue location
  , venue_title :: Text                 -- ^ Name of the venue
  , venue_address :: Text               -- ^ Address of the venue
  , venue_foursquare_id :: Maybe Text   -- ^ Foursquare identifier of the venue
  } deriving (Show, Generic)

instance ToJSON Venue where
  toJSON = toJsonDrop 6

instance FromJSON Venue where
  parseJSON = parseJsonDrop 6

data KeyboardButton = KeyboardButton
  {
    kb_text             :: Text -- ^ Text of the button. If none of the optional fields are used, it will be sent to the bot as a message when the button is pressed
  , kb_request_contact  :: Maybe Bool -- ^ If True, the user's phone number will be sent as a contact when the button is pressed. Available in private chats only
  , kb_request_location :: Maybe Bool -- ^ If True, the user's current location will be sent when the button is pressed. Available in private chats only
  } deriving (Show, Generic)

instance ToJSON KeyboardButton where
  toJSON = toJsonDrop 3

instance FromJSON KeyboardButton where
  parseJSON = parseJsonDrop 3

keyboardButton :: Text -> KeyboardButton
keyboardButton buttonText = KeyboardButton buttonText Nothing Nothing

data WebhookInfo = WebhookInfo
  {
    whi_url :: Text -- ^ Webhook URL, may be empty if webhook is not set up
  , whi_has_custom_certificate :: Bool -- ^ True, if a custom certificate was provided for webhook certificate checks
  , whi_pending_update_count :: Int -- ^ Number of updates awaiting delivery
  , whi_last_error_date :: Maybe Int -- ^ Unix time for the most recent error that happened when trying to deliver an update via webhook
  , whi_last_error_message :: Maybe Text -- ^ Error message in human-readable format for the most recent error that happened when trying to deliver an update via webhook
  } deriving (Show, Generic)

instance ToJSON WebhookInfo where
  toJSON = toJsonDrop 4

instance FromJSON WebhookInfo where
  parseJSON = parseJsonDrop 4