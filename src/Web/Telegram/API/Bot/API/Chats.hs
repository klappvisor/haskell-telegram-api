{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Web.Telegram.API.Bot.API.Chats
  ( -- * Functions
    kickChatMember
  , kickChatMemberM
  , leaveChat
  , leaveChatM
  , unbanChatMember
  , unbanChatMemberM
  , getChat
  , getChatM
  , getChatAdministrators
  , getChatAdministratorsM
  , getChatMembersCount
  , getChatMembersCountM
  , getChatMember
  , getChatMemberM
    -- * API
  , TelegramBotChatsAPI
  , chatsApi
  ) where

import           Data.Proxy
import           Data.Text                        (Text)
import           Network.HTTP.Client              (Manager)
import           Servant.API
import           Servant.Client
import           Web.Telegram.API.Bot.API.Core
import           Web.Telegram.API.Bot.Responses

-- | Telegram Bot API
type TelegramBotChatsAPI =
         TelegramToken :> "kickChatMember"
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

-- | Proxy for Thelegram Bot API to administrate chats
chatsApi :: Proxy TelegramBotChatsAPI
chatsApi = Proxy

kickChatMember_            :: Token -> Maybe Text -> Maybe Int -> ClientM KickChatMemberResponse
leaveChat_                 :: Token -> Maybe Text -> ClientM LeaveChatResponse
unbanChatMember_           :: Token -> Maybe Text -> Maybe Int -> ClientM UnbanChatMemberResponse
getChat_                   :: Token -> Maybe Text -> ClientM GetChatResponse
getChatAdministrators_     :: Token -> Maybe Text -> ClientM GetChatAdministratorsResponse
getChatMembersCount_       :: Token -> Maybe Text -> ClientM GetChatMembersCountResponse
getChatMember_             :: Token -> Maybe Text -> Maybe Int -> ClientM GetChatMemberResponse
kickChatMember_
  :<|> leaveChat_
  :<|> unbanChatMember_
  :<|> getChat_
  :<|> getChatAdministrators_
  :<|> getChatMembersCount_
  :<|> getChatMember_ = client chatsApi

-- | Use this method to kick a user from a group or a supergroup. In the case of supergroups, the user will not be able to return to the group on their own using invite links, etc., unless unbanned first. The bot must be an administrator in the group for this to work.
kickChatMember :: Token -> Text -> Int -> Manager -> IO (Either ServantError KickChatMemberResponse)
kickChatMember token chatId userId = runClient (kickChatMemberM chatId userId) token

-- | See 'kickChatMember'
kickChatMemberM :: Text -> Int -> TelegramClient KickChatMemberResponse
kickChatMemberM chatId userId = asking $ \t -> kickChatMember_ t (Just chatId) (Just userId)

-- | Use this method for your bot to leave a group, supergroup or channel. Returns True on success.
leaveChat :: Token -> Text -> Manager -> IO (Either ServantError LeaveChatResponse)
leaveChat = runM leaveChatM

-- | See 'leaveChat'
leaveChatM :: Text -> TelegramClient LeaveChatResponse
leaveChatM chatId = run_ leaveChat_ (Just chatId)

-- | Use this method to unban a previously kicked user in a supergroup. The user will not return to the group automatically, but will be able to join via link, etc. The bot must be an administrator in the group for this to work.
unbanChatMember :: Token -> Text -> Int -> Manager -> IO (Either ServantError UnbanChatMemberResponse)
unbanChatMember token chatId userId = runClient (unbanChatMemberM chatId userId) token

-- | See 'unbanChatMember'
unbanChatMemberM :: Text -> Int -> TelegramClient UnbanChatMemberResponse
unbanChatMemberM chatId userId = asking $ \t -> unbanChatMember_ t (Just chatId) (Just userId)

-- | Use this method to get up to date information about the chat (current name of the user for one-on-one conversations, current username of a user, group or channel, etc.)
getChat :: Token -> Text -> Manager -> IO (Either ServantError GetChatResponse)
getChat = runM getChatM

-- | See 'getChat'
getChatM :: Text -> TelegramClient GetChatResponse
getChatM chatId = run_ getChat_ (Just chatId)

-- | Use this method to get a list of administrators in a chat. On success, returns an Array of 'ChatMember' objects that contains information about all chat administrators except other bots. If the chat is a group or a supergroup and no administrators were appointed, only the creator will be returned.
getChatAdministrators :: Token -> Text -> Manager -> IO (Either ServantError GetChatAdministratorsResponse)
getChatAdministrators = runM getChatAdministratorsM

-- | See 'getChatAdministrators'
getChatAdministratorsM :: Text -> TelegramClient GetChatAdministratorsResponse
getChatAdministratorsM chatId = run_ getChatAdministrators_ (Just chatId)

-- | Use this method to get the number of members in a chat. Returns 'Int' on success.
getChatMembersCount :: Token -> Text -> Manager -> IO (Either ServantError GetChatMembersCountResponse)
getChatMembersCount = runM getChatMembersCountM

-- | See 'getChatMembersCount'
getChatMembersCountM :: Text -> TelegramClient GetChatMembersCountResponse
getChatMembersCountM chatId = run_ getChatMembersCount_ (Just chatId)

-- | Use this method to get information about a member of a chat. Returns a 'ChatMember' object on success.
getChatMember :: Token -> Text -> Int -> Manager -> IO (Either ServantError GetChatMemberResponse)
getChatMember token chatId userId = runClient (getChatMemberM chatId userId) token

-- | See 'getChatMember'
getChatMemberM :: Text -> Int -> TelegramClient GetChatMemberResponse
getChatMemberM chatId userId = asking $ \t -> getChatMember_ t (Just chatId) (Just userId)
