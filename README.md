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
import Control.Monad
import qualified Data.Text.IO as T
import Data.Maybe
import Web.Telegram.API.Bot

main :: IO ()
main = do
  Right GetMeResponse { user_result = u } <-
    getMe token
  T.putStrLn (user_first_name u)
  where token = Token "bot<token>" -- entire Token should be bot123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11
```

`sendMessage` example

```haskell
import Control.Monad
import qualified Data.Text.IO as T
import Data.Maybe
import Web.Telegram.API.Bot

main :: IO ()
main = do
  Right MessageResponse { message_result = m } <-
    sendMessage token (SendMessageRequest chatId message (Just Markdown) Nothing Nothing Nothing)
  T.putStrLn (message_id m)
  T.putStrLn (text m)
  where token = Token "bot<token>" -- entire Token should be bot123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11
        chatId = "<chat_id> or <@channelusername>" 
        message = "text *bold* _italic_ [github](github.com/klappvisor/haskell-telegram-api)"
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

* `$BOT_TOKEN` is token obtained from BotFather with prefix `bot<token from BotFather>`
* `$CHAT_ID` can be id of your chat with your bot. Send some message to this chat in Telegram and do `curl "https://api.telegram.org/bot<replace_with_token>/getUpdates"`, you have to parse some JSON with your brain ;-) or any other suitable tool and you will find chat id there.
* `$BOT_NAME` name of your bot

Note: Inline Spec is disabled for now...

If everything is fine after test you will see receive a few new messages from your bot.

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
