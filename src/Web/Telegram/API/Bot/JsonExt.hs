{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes  #-}

-- | This module contains helper functions to work with JSON
module Web.Telegram.API.Bot.JsonExt
    (
      toJsonDrop,
      parseJsonDrop
    ) where

import           Data.Aeson
import           Data.Aeson.Types
import           GHC.Generics

-- | Method used to drop prefix from field name during serialization
toJsonDrop :: forall a.(GHC.Generics.Generic a, GToJSON (GHC.Generics.Rep a)) => Int -> a -> Value
toJsonDrop prefix = genericToJSON defaultOptions {
    fieldLabelModifier = drop prefix
  , omitNothingFields = True
  }

-- | Method used to drop prefix from field name during deserialization
parseJsonDrop :: forall a.(Generic a, GFromJSON (Rep a)) => Int -> Value -> Parser a
parseJsonDrop prefix = genericParseJSON defaultOptions { fieldLabelModifier = drop prefix }
