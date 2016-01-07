# telegram-api

![Build Status](https://img.shields.io/circleci/project/klappvisor/haskell-telegram-api.svg)
![Hackage](https://img.shields.io/hackage/v/telegram-api.svg)
![Hackage Dependencies](https://img.shields.io/hackage-deps/v/telegram-api.svg)
![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)
![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)

High-level bindings to the [Telegram Bots API][telegram-bot-api] based on [servant][servant] library. 
Currently supports only one way of receiving updates based on [`getUpdates`](https://core.telegram.org/bots/api#getupdates) method.
Uploading stickers, documents, video, etc is not supported yet, so you can only send items which are already uploaded on the Telegram servers.
See list of supported methods below in TODO section.

## Usage

`getMe` example

```haskell
import Control.Monad
import qualified Data.Text.IO as T
import Data.Maybe
import Telegram.API.Data
import Telegram.API.Responses
import Telegram.API.Bot

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
import Telegram.API.Data
import Telegram.API.Requests
import Telegram.API.Responses
import Telegram.API.Bot

main :: IO ()
main = do
  Right MessageResponse { message_result = m } <-
    sendMessage token (SendMessageRequest chatId message (Just Markdown) Nothing Nothing)
    T.putStrLn (message_id m)
    T.putStrLn (text m)
  where token = Token "bot<token>" -- entire Token should be bot123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11
        chatId = "<chat_id> or <@channelusername>" 
        message = "text *bold* _italic_ [github](github.com/klappvisor/haskell-telegram-api)"
```

## TODO

### General

* `reply_markup` is skipped for all methods 
* Inline mode is not supported yet
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

##### To be done:

* `sendPhoto` - upload photo
* `sendAudio` - upload audio
* `sendDocument` - upload documents
* `sendSticker` - upload stickers
* `sendVideo` - upload video
* `sendVoice` - upload voice
* `setWebhook` - upload certificate
* `answerInlineQuery` inline bots

[telegram-bot-api]: https://core.telegram.org/bots/api
[servant]: https://haskell-servant.github.io/
