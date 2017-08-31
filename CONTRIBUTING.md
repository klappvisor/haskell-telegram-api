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
