{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | This module provides Telegram Bot API
module Telegram.API.Bot
  (
    module Telegram.API.Bot.API
  , module Telegram.API.Bot.Data
  , module Telegram.API.Bot.Responses
  , module Telegram.API.Bot.Requests
  ) where

import           Telegram.API.Bot.API
import           Telegram.API.Bot.Data
import           Telegram.API.Bot.Responses
import           Telegram.API.Bot.Requests