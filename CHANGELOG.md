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
