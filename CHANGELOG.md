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
