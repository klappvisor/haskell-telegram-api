{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Web.Telegram.API.Bot.API.Chats
  ( -- * Functions
    kickChatMember
  , leaveChat
  , unbanChatMember
  , getChat
  , getChatAdministrators
  , getChatMembersCount
  , getChatMember
    -- * API
  , TelegramBotChatsAPI
  , chatsApi
  ) where

import           Data.Proxy
import           Data.Text (Text)
import           Network.HTTP.Client (Manager)
import           Servant.API
import           Servant.Client
import           Servant.Client.MultipartFormData
import           Web.Telegram.API.Bot.Responses
import           Web.Telegram.API.Bot.Requests
import           Web.Telegram.API.Bot.API.Core

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
