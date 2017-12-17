{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module TestCore (success, nosuccess, showText) where

import           Data.Either (isLeft, isRight)
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Test.Hspec

-- to print out remote response if response success not match
success, nosuccess :: (Show a, Show b) => Either a b -> Expectation
success   e = e `shouldSatisfy` isRight
nosuccess e = e `shouldSatisfy` isLeft

showText :: (Show a) => a -> Text
showText = T.pack . show
