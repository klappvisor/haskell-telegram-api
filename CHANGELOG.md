## 0.7.1.0

* Added support up to Bot API v3.5: sticker sets, chats administration, live location, etc
* Added `runTelegramClient` with different than in `runClient` order of arguments. Now possible to write `runTelegramClient token manager $ do`
* Stop adding request specific responses
* Stop adding IO calls, use functions with `M` suffix, for example `runClient getWebhookInfoM`

## 0.7.0.0

* Upgraded to servant-0.11 

## 0.6.3.0

* New fields *gif_duration* in `InlineQueryResultGif` and *mpeg4_duration* in `InlineQueryResultMpeg4Gif`.
* Replaced the field *new_chat_member* in `Message` with *new_chat_members* (the old field will still be available for a while for compatibility purposes).
* The `User` object now may have a *language_code* field that contains the IETF language tag of the user's language.
* Added the `sendVideoNote` method, the new field *video_note* to `Message`, the fields `RecordVideoNote* or *UploadVideoNote* to `sendChatAction`.
* Added a new type of button, the pay button to `InlineKeyboardButton`.
* Updated dependencies

## 0.6.2.0

* Added new kinds of updates, *shipping_query* and *pre_checkout_query*, and new types of message content, *invoice* and *successful_payment*.
* Added new methods for payments: `sendInvoiceM`, `answerShippingQueryM`, and `answerPreCheckoutQueryM`.

## 0.6.1.1

Bugfixes:

* Migration to `Int64` to represent chat id and fix integer overflow issue

## 0.6.1.0

* Added `ChatId` data type since it can be integer or string starting from `@`, f.e. `@channelusername`
* Changes in `getUpdates` and `getUpdateM` function

## 0.6.0.1

* Bump aeson upper bound to include 1.1.*

## 0.6.0.0

* Added `TelegramClient`, see example of usage in README.md
* Changes from December update of Telegram Bot API
* `TelegramBotAPI` splitted in separate sub-types  

## 0.5.2.0

Features:

* Added webhook methods such as getWebhookInfo, deleteWebhook, etc.
* Changes to update api
* Added sendGame  

## 0.5.1.1

Updated to use servant-0.9 and aeson-1.0

## 0.5.0.0 [Breaking]

Features:

* **[Breaking]** Changed `Response` data record to be generic
* Added certificate upload to set web-hook method for self-signed certificates

Bugfixes:

* Removed `O2` tag from cabal file

## 0.4.3.1

Bugfixes:

* Exposed `MessageEntity`

## 0.4.3.0

Features:

* Added Inline Keyboard to messages

## 0.4.2.0

Features:

* Bot-2.1 support
  * Added new methods: `getChat`, `leaveChat`, `getChatAdministrators`, `getChatMember`, `getChatMembersCount`.
  * Added support for edited messages and new mentions from Telegram v.3.9. New fields: `edited_message` in `Update`, `edit_date` in `Message, user in `MessageEntity`. New value `text_mention` for the type field in `MessageEntity`.

## 0.4.1.0

Features:

* Implemented file uploading for audio, voice, sticker, video and documents

Bugfixes:

* Exposed constrictors for inline edit requests

## 0.4.0.1

Bugfixes:

* Parsing issue with answer callback query response

## 0.4.0.0 [Breaking]

Features:

* Bot-2.0 API support is almost complete. Everything from [announce](https://core.telegram.org/bots/2-0-intro) is on place except file upload (works for photo BTW).
  * Inline keyboards
  * Updating messages
  * Send Location and Phone number
  * Inline Bots 2.0
  * Group Admins
  * and many others
* Added new and more convenient way to create request data records avoiding many optional parameters.
* migrated to servant 0.7.*

## 0.3.1.0

Features:

* Added possibility to upload and send photo

## 0.3.0.0

Bugfixes:

* *[Breaking]* Changed User to be optional in `from` field of the `Message` data record since sender can be empty for messages sent to channels

## 0.2.1.1

Bugfixes:

* Minor fix of chat action deserialization code

## 0.2.1.0

Features:

* Added reply keyboard

## 0.2.0.0

Features:

* Main functionality is on place except content upload.
* Inline mode added
