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

**Support of [Bot-3.5 API][bot-api]**

## Usage

There are two ways of using Telegram Bot API. First and original way is to run IO directly for every Telegram servers request, another one is based on `TelegramClient` which is just `ReaderT`.

### Use `TelegramClient`

```haskell
{-# LANGUAGE OverloadedStrings #-}

import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Web.Telegram.API.Bot

main :: IO ()
main = do
  let token = Token "bot<token>" -- entire Token should be bot123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11
  manager <- newManager tlsManagerSettings
  result <- runTelegramClient token manager $ do
    info <- getWebhookInfoM
    let request = setWebhookRequest' "https://example.com/hook"
    isSet <- setWebhookM request
    getMeM
  print result
  print "done!"
```

### Running IO directly (planning to deprecate this option)
:warning: This method to interact with a Telegram bot is about to be depricated and all new API calls will only have their `M` versions. You can run them using `runTelegramClient` function, for example `runTelegramClient token manager $ sendMessageM message` or in example below replace `getMe token manager` with `runTelegramClient token manager getMeM` to get the same behavior.

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
    Right Response { result = u } -> do
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
    Right Response { result = m } -> do
      putStrLn "Request succeded"
      print $ message_id m
      print $ text m
  where token = Token "bot<token>" -- entire Token should be bot123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11
        chatId = ChatId <chat_id> -- use ChatId 10231 or ChatChannel "<@channelusername>"
        message = "text *bold* _italic_ [github](github.com/klappvisor/haskell-telegram-api)"
```

#### Note on requests:

Many request data records have a lot of optional parameters which are usually redundant.
There are two ways to create requests:

With data type constructor:
```haskell
let request = SendMessageRequest (ChatId int64_chatId) "text" Nothing (Just True) Nothing Nothing Nothing
```
Using default instance:

```haskell
let request = sendMessageRequest (ChatId int64_chatId) "text" -- only with required fields
```

```haskell
let request = (sendMessageRequest ChatId int64_chatId) "text") {
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

Bear in mind that the CI build won't run integration test suite against your pull request since the necessary environment
variables (`$BOT_TOKEN`, `$STRIPE_TOKEN`, `$CHAT_ID` and `$BOT_NAME`) aren't exported when a fork
starts the build (for security reasons). If you do want to run them before creating RP, you can setup integration of your fork
with CircleCI.

You can use `stack` to build project

```
stack build
```

To run test you have to create your own bot. Go to [BotFather](https://telegram.me/botfather) and create the bot. As the result you will have private bot's access token. Keep it safe!

```
stack test --test-arguments "--integration -c CHAT_ID -b BOT_NAME -- HSPEC_ARGS"
```

where

* `BOT_TOKEN` is the token obtained from BotFather and must be defined as environment variable
* `PAYMENT_TOKEN` is the token obtained from BotFather and must be defined as environment variable
* `CHAT_ID` can be id of your chat with your bot. Send some messages to this chat in Telegram and do `curl "https://api.telegram.org/bot<replace_with_token>/getUpdates"`, you'll have to parse some JSON with your brain ;-) or any other suitable tool and you will find chat id there.
* `BOT_NAME` is the name of your bot
* `HSPEC_ARGS` are the normal `hspec` arguments you can find [here][hspec-args]

The help option is available for the tests and for hspec:

```
stack test --test-arguments "-h"
stack test --test-arguments "--integration -c CHAT_ID -b BOT_NAME -- -h"
```

Note: Inline Spec is disabled for now...

If everything is fine after running the tests you will receive a few new messages from your bot.

[telegram-bot-api]: https://core.telegram.org/bots/api
[servant]: https://haskell-servant.github.io/
[hspec-args]: https://hspec.github.io/running-specs.html
[bot-api]: https://core.telegram.org/bots/api
