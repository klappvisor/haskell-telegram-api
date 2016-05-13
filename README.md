# telegram-api

[![Join the chat at https://gitter.im/klappvisor/haskell-telegram-api](https://badges.gitter.im/klappvisor/haskell-telegram-api.svg)](https://gitter.im/klappvisor/haskell-telegram-api?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

![Build Status](https://img.shields.io/circleci/project/klappvisor/haskell-telegram-api.svg)
![Hackage](https://img.shields.io/hackage/v/telegram-api.svg)
![Hackage Dependencies](https://img.shields.io/hackage-deps/v/telegram-api.svg)
![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)
![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)

High-level bindings to the [Telegram Bot API][telegram-bot-api] based on [servant][servant] library. 
Both `getUpdates` request or webhook can be used to receive updates for your bot. 
Inline mode is supported.
Uploading stickers, documents, video, etc is not supported yet, but you can send items which are already uploaded on the Telegram servers.

See list of supported methods below in TODO section.

## Usage

`getMe` example

```haskell
{-# LANGUAGE OverloadedStrings #-}

import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Web.Telegram.API.Bot

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  res <- getMe token manager
  case res of
    Left e -> do
      putStrLn "Request failed"
      print e
    Right GetMeResponse { user_result = u } -> do
      putStrLn "Request succeded"
      print $ user_first_name u
  where token = Token "bot<token>" -- entire Token should be bot123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11
```

`sendMessage` example

```haskell
{-# LANGUAGE OverloadedStrings #-}

import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Web.Telegram.API.Bot

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  let request = sendMessageRequest chatId message
  res <- sendMessage token request manager
  case res of
    Left e -> do
      putStrLn "Request failed"
      print e
    Right MessageResponse { message_result = m } -> do
      putStrLn "Request succeded"
      print $ message_id m
      print $ text m
  where token = Token "bot<token>" -- entire Token should be bot123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11
        chatId = "<chat_id> or <@channelusername>" 
        message = "text *bold* _italic_ [github](github.com/klappvisor/haskell-telegram-api)"
```

#### Note on requests:

Many request data records have a lot of optional parameters which are usually redundant.
There are two ways to create requests:

With data type constructor:
```haskell
let request = SendMessageRequest "chatId" "text" Nothing (Just True) Nothing Nothing Nothing
```
Using default instance:

```haskell
let request = sendMessageRequest "chatId" "text" -- only with required fields
```

```haskell
let request = (sendMessageRequest "chatId" "text") {
  message_disable_notification = Just True -- with optional fields
}
```

## Contribution

Contributions are welcome!

1. Fork repository
2. Do some changes
3. Create pull request
4. Wait for CI build and review
5. ??????
6. PROFIT

You can use `stack` to build project

```
stack build
```

To run test you have to create your own bot. Go to [BotFather](https://telegram.me/botfather) and create the bot. As the result you will have private bot's access token. Keep it safe!

```
stack test --test-arguments "$BOT_TOKEN $CHAT_ID $BOT_NAME"
```

where

* `$BOT_TOKEN` is token obtained from BotFather
* `$CHAT_ID` can be id of your chat with your bot. Send some message to this chat in Telegram and do `curl "https://api.telegram.org/bot<replace_with_token>/getUpdates"`, you have to parse some JSON with your brain ;-) or any other suitable tool and you will find chat id there.
* `$BOT_NAME` name of your bot

Note: Inline Spec is disabled for now...

If everything is fine after running the tests you will receive a few new messages from your bot.

## TODO

* Uploading of Files, Documents, Stickers, etc

### Methods

##### Currently supported:

* `getMe`
* `sendMessage`
* `forwardMessage`
* `sendPhoto` - without upload
* `sendAudio` - without upload
* `sendDocument` - without upload
* `sendSticker` - without upload
* `sendVideo` - without upload
* `sendVoice` - without upload
* `sendLocation`
* `getUpdates`
* `getFile`
* `sendChatAction`
* `getUserProfilePhotos`
* `setWebhook` - without uploading certificate
* `answerInlineQuery`

##### To be done:

* `sendPhoto` - upload photo
* `sendAudio` - upload audio
* `sendDocument` - upload documents
* `sendSticker` - upload stickers
* `sendVideo` - upload video
* `sendVoice` - upload voice
* `setWebhook` - upload certificate

[telegram-bot-api]: https://core.telegram.org/bots/api
[servant]: https://haskell-servant.github.io/
