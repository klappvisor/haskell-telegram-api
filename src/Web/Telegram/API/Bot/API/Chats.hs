{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Web.Telegram.API.Bot.API.Chats
  ( -- * Functions
    kickChatMember
  , kickChatMemberM
  , kickChatMemberUntilM
  , leaveChat
  , leaveChatM
  , unbanChatMember
  , unbanChatMemberM
  , restrictChatMemberM
  , promoteChatMemberM
  , exportChatInviteLinkM
  , setChatPhotoM
  , deleteChatPhotoM
  , setChatTitleM
  , setChatDescriptionM
  , pinChatMessageM
  , unpinChatMessageM
  , getChat
  , getChatM
  , getChatAdministrators
  , getChatAdministratorsM
  , getChatMembersCount
  , getChatMembersCountM
  , getChatMember
  , getChatMemberM
  , setChatStickerSetM
  , deleteChatStickerSetM
    -- * API
  , TelegramBotChatsAPI
  , chatsApi
  ) where

import           Data.Proxy
import           Data.Text                        (Text)
import           Network.HTTP.Client              (Manager)
import           Servant.API
import           Servant.Client            hiding (Response)
import           Servant.Client.MultipartFormData
import           Web.Telegram.API.Bot.API.Core
import           Web.Telegram.API.Bot.Requests
import           Web.Telegram.API.Bot.Responses

-- | Telegram Bot API
type TelegramBotChatsAPI =
         TelegramToken :> "kickChatMember"
         :> QueryParam "chat_id" Text
         :> QueryParam "user_id" Int
         :> QueryParam "until_date" Int
         :> Post '[JSON] KickChatMemberResponse
    :<|> TelegramToken :> "leaveChat"
         :> QueryParam "chat_id" Text
         :> Post '[JSON] LeaveChatResponse
    :<|> TelegramToken :> "unbanChatMember"
         :> QueryParam "chat_id" Text
         :> QueryParam "user_id" Int
         :> Post '[JSON] UnbanChatMemberResponse
    :<|> TelegramToken :> "restrictChatMember"
         :> ReqBody '[JSON] RestrictChatMemberRequest
         :> Post '[JSON] (Response Bool)
    :<|> TelegramToken :> "promoteChatMember"
         :> ReqBody '[JSON] PromoteChatMemberRequest
         :> Post '[JSON] (Response Bool)
    :<|> TelegramToken :> "exportChatInviteLink"
         :> QueryParam "chat_id" Text
         :> Post '[JSON] (Response Text)
    :<|> TelegramToken :> "setChatPhoto"
         :> MultipartFormDataReqBody SetChatPhotoRequest
         :> Post '[JSON] (Response Bool)
    :<|> TelegramToken :> "deleteChatPhoto"
         :> QueryParam "chat_id" Text
         :> Post '[JSON] (Response Bool)
    :<|> TelegramToken :> "setChatTitle"
         :> QueryParam "chat_id" Text
         :> QueryParam "title" Text
         :> Post '[JSON] (Response Bool)
    :<|> TelegramToken :> "setChatDescription"
         :> QueryParam "chat_id" Text
         :> QueryParam "description" Text
         :> Post '[JSON] (Response Bool)
    :<|> TelegramToken :> "pinChatMessage"
         :> QueryParam "chat_id" Text
         :> QueryParam "message_id" Int
         :> QueryParam "disable_notification" Bool
         :> Post '[JSON] (Response Bool)
    :<|> TelegramToken :> "unpinChatMessage"
         :> QueryParam "chat_id" Text
         :> Post '[JSON] (Response Bool)
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
    :<|> TelegramToken :> "setChatStickerSet"
         :> QueryParam "chat_id" Text
         :> QueryParam "sticker_set_name" Text
         :> Post '[JSON] (Response Bool)
    :<|> TelegramToken :> "deleteChatStickerSet"
         :> QueryParam "chat_id" Text
         :> Post '[JSON] (Response Bool)

-- | Proxy for Thelegram Bot API to administrate chats
chatsApi :: Proxy TelegramBotChatsAPI
chatsApi = Proxy

kickChatMember_            :: Token -> Maybe Text -> Maybe Int -> Maybe Int -> ClientM KickChatMemberResponse
leaveChat_                 :: Token -> Maybe Text -> ClientM LeaveChatResponse
unbanChatMember_           :: Token -> Maybe Text -> Maybe Int -> ClientM UnbanChatMemberResponse
restrictChatMember_        :: Token -> RestrictChatMemberRequest -> ClientM (Response Bool)
promoteChatMember_         :: Token -> PromoteChatMemberRequest -> ClientM (Response Bool)
exportChatInviteLink_      :: Token -> Maybe Text -> ClientM (Response Text)
setChatPhoto_              :: Token -> SetChatPhotoRequest -> ClientM (Response Bool)
deleteChatPhoto_           :: Token -> Maybe Text -> ClientM (Response Bool)
setChatTitle_              :: Token -> Maybe Text -> Maybe Text -> ClientM (Response Bool)
setChatDescription_        :: Token -> Maybe Text -> Maybe Text -> ClientM (Response Bool)
pinChatMessage_            :: Token -> Maybe Text -> Maybe Int -> Maybe Bool -> ClientM (Response Bool)
unpinChatMessage_          :: Token -> Maybe Text -> ClientM (Response Bool)
getChat_                   :: Token -> Maybe Text -> ClientM GetChatResponse
getChatAdministrators_     :: Token -> Maybe Text -> ClientM GetChatAdministratorsResponse
getChatMembersCount_       :: Token -> Maybe Text -> ClientM GetChatMembersCountResponse
getChatMember_             :: Token -> Maybe Text -> Maybe Int -> ClientM GetChatMemberResponse
setChatStickerSet_         :: Token -> Maybe Text -> Maybe Text -> ClientM (Response Bool)
deleteChatStickerSet_      :: Token -> Maybe Text -> ClientM (Response Bool)
kickChatMember_
  :<|> leaveChat_
  :<|> unbanChatMember_
  :<|> restrictChatMember_
  :<|> promoteChatMember_
  :<|> exportChatInviteLink_
  :<|> setChatPhoto_
  :<|> deleteChatPhoto_
  :<|> setChatTitle_
  :<|> setChatDescription_
  :<|> pinChatMessage_
  :<|> unpinChatMessage_
  :<|> getChat_
  :<|> getChatAdministrators_
  :<|> getChatMembersCount_
  :<|> getChatMember_
  :<|> setChatStickerSet_
  :<|> deleteChatStickerSet_
    = client chatsApi

-- | Use this method to kick a user from a group or a supergroup. In the case of supergroups, the user will not be able to return to the group on their own using invite links, etc., unless unbanned first. The bot must be an administrator in the group for this to work.
kickChatMember :: Token
    -> Text -- ^ Unique identifier for the target group or username of the target supergroup or channel (in the format @channelusername)
    -> Int -- ^ Unique identifier of the target user
    -> Manager -> IO (Either ClientError KickChatMemberResponse)
kickChatMember token chatId userId = runClient (kickChatMemberM chatId userId) token

-- | See 'kickChatMember'
kickChatMemberM :: Text -> Int -> TelegramClient KickChatMemberResponse
kickChatMemberM chatId userId = asking $ \t -> kickChatMember_ t (Just chatId) (Just userId) Nothing

kickChatMemberUntilM :: Text -- ^ Unique identifier for the target group or username of the target supergroup or channel (in the format @channelusername)
    -> Int -- ^ Unique identifier of the target user
    -> Int -- ^ Date when the user will be unbanned, unix time. If user is banned for more than 366 days or less than 30 seconds from the current time they are considered to be banned forever
    -> TelegramClient KickChatMemberResponse
kickChatMemberUntilM chatId userId untilDate = asking $ \t -> kickChatMember_ t (Just chatId) (Just userId) (Just untilDate)

-- | Use this method for your bot to leave a group, supergroup or channel. Returns True on success.
leaveChat :: Token -> Text -> Manager -> IO (Either ClientError LeaveChatResponse)
leaveChat = runM leaveChatM

-- | See 'leaveChat'
leaveChatM :: Text -> TelegramClient LeaveChatResponse
leaveChatM chatId = run_ leaveChat_ (Just chatId)

-- | Use this method to unban a previously kicked user in a supergroup. The user will not return to the group automatically, but will be able to join via link, etc. The bot must be an administrator in the group for this to work.
unbanChatMember :: Token -> Text -> Int -> Manager -> IO (Either ClientError UnbanChatMemberResponse)
unbanChatMember token chatId userId = runClient (unbanChatMemberM chatId userId) token

-- | See 'unbanChatMember'
unbanChatMemberM :: Text -> Int -> TelegramClient UnbanChatMemberResponse
unbanChatMemberM chatId userId = asking $ \t -> unbanChatMember_ t (Just chatId) (Just userId)

-- | Use this method to restrict a user in a supergroup. The bot must be an administrator in the supergroup for this to work and must have the appropriate admin rights. Pass True for all boolean parameters to lift restrictions from a user. Returns True on success.
restrictChatMemberM :: RestrictChatMemberRequest -> TelegramClient (Response Bool)
restrictChatMemberM = run_ restrictChatMember_

-- | Use this method to promote or demote a user in a supergroup or a channel. The bot must be an administrator in the chat for this to work and must have the appropriate admin rights. Pass False for all boolean parameters to demote a user. Returns True on success.
promoteChatMemberM :: PromoteChatMemberRequest -> TelegramClient (Response Bool)
promoteChatMemberM = run_ promoteChatMember_

-- | Use this method to export an invite link to a supergroup or a channel. The bot must be an administrator in the chat for this to work and must have the appropriate admin rights. Returns exported invite link as String on success.
exportChatInviteLinkM :: Text -> TelegramClient (Response Text)
exportChatInviteLinkM chatId = run_ exportChatInviteLink_ (Just chatId)

-- | Use this method to set a new profile photo for the chat. Photos can't be changed for private chats. The bot must be an administrator in the chat for this to work and must have the appropriate admin rights. Returns True on success.
setChatPhotoM :: SetChatPhotoRequest -> TelegramClient (Response Bool)
setChatPhotoM = run_ setChatPhoto_

-- | Use this method to delete a chat photo. Photos can't be changed for private chats. The bot must be an administrator in the chat for this to work and must have the appropriate admin rights. Returns True on success.
deleteChatPhotoM :: Text -> TelegramClient (Response Bool)
deleteChatPhotoM chatId = run_ deleteChatPhoto_ (Just chatId)

-- | Use this method to change the title of a chat. Titles can't be changed for private chats. The bot must be an administrator in the chat for this to work and must have the appropriate admin rights. Returns True on success.
setChatTitleM :: Text -> Maybe Text -> TelegramClient (Response Bool)
setChatTitleM chatId title = asking $ \t -> setChatTitle_ t (Just chatId) title

-- | Use this method to change the description of a supergroup or a channel. The bot must be an administrator in the chat for this to work and must have the appropriate admin rights. Returns True on success.
setChatDescriptionM :: Text -> Maybe Text -> TelegramClient (Response Bool)
setChatDescriptionM chatId description = asking $ \t -> setChatDescription_ t (Just chatId) description

-- | Use this method to pin a message in a supergroup or a channel. The bot must be an administrator in the chat for this to work and must have the 'can_pin_messages' admin right in the supergroup or 'can_edit_messages' admin right in the channel. Returns True on success.
pinChatMessageM :: Text -> Int -> Maybe Bool -> TelegramClient (Response Bool)
pinChatMessageM chatId messageId disableNotifications = asking $ \tkn -> pinChatMessage_ tkn (Just chatId) (Just messageId) disableNotifications

-- | Use this method to unpin a message in a supergroup or a channel. The bot must be an administrator in the chat for this to work and must have the 'can_pin_messages' admin right in the supergroup or 'can_edit_messages' admin right in the channel. Returns True on success.
unpinChatMessageM :: Text -> TelegramClient (Response Bool)
unpinChatMessageM chatId = run_ unpinChatMessage_ (Just chatId)

-- | Use this method to get up to date information about the chat (current name of the user for one-on-one conversations, current username of a user, group or channel, etc.)
getChat :: Token -> Text -> Manager -> IO (Either ClientError GetChatResponse)
getChat = runM getChatM

-- | See 'getChat'
getChatM :: Text -> TelegramClient GetChatResponse
getChatM chatId = run_ getChat_ (Just chatId)

-- | Use this method to get a list of administrators in a chat. On success, returns an Array of 'ChatMember' objects that contains information about all chat administrators except other bots. If the chat is a group or a supergroup and no administrators were appointed, only the creator will be returned.
getChatAdministrators :: Token -> Text -> Manager -> IO (Either ClientError GetChatAdministratorsResponse)
getChatAdministrators = runM getChatAdministratorsM

-- | See 'getChatAdministrators'
getChatAdministratorsM :: Text -> TelegramClient GetChatAdministratorsResponse
getChatAdministratorsM chatId = run_ getChatAdministrators_ (Just chatId)

-- | Use this method to get the number of members in a chat. Returns 'Int' on success.
getChatMembersCount :: Token -> Text -> Manager -> IO (Either ClientError GetChatMembersCountResponse)
getChatMembersCount = runM getChatMembersCountM

-- | See 'getChatMembersCount'
getChatMembersCountM :: Text -> TelegramClient GetChatMembersCountResponse
getChatMembersCountM chatId = run_ getChatMembersCount_ (Just chatId)

-- | Use this method to get information about a member of a chat. Returns a 'ChatMember' object on success.
getChatMember :: Token -> Text -> Int -> Manager -> IO (Either ClientError GetChatMemberResponse)
getChatMember token chatId userId = runClient (getChatMemberM chatId userId) token

-- | See 'getChatMember'
getChatMemberM :: Text -> Int -> TelegramClient GetChatMemberResponse
getChatMemberM chatId userId = asking $ \t -> getChatMember_ t (Just chatId) (Just userId)

setChatStickerSetM :: Text -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
    -> Text -- ^ Name of the sticker set to be set as the group sticker set
    -> TelegramClient (Response Bool)
setChatStickerSetM chatId stickerSetName = asking $ \t -> setChatStickerSet_ t (Just chatId) (Just stickerSetName)

deleteChatStickerSetM :: Text -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
    -> TelegramClient (Response Bool)
deleteChatStickerSetM chatId = run_ deleteChatStickerSet_ (Just chatId)
