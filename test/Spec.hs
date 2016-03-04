{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where

import           Control.Monad
import           Web.Telegram.API.Bot
import           Test.Hspec
import           Data.Text (Text)
import qualified Data.Text as T
import           Servant.Client
import           Servant.API
import           Network.HTTP.Types.Status
import           System.Environment
import qualified MainSpec
import qualified InlineSpec

main :: IO ()
main = do
    args <- getArgs
    withArgs [] $ hspec (runSpec args)

-- Don't run integration tests if no token and chat id provided
runSpec :: [String] -> SpecWith ()
runSpec [] = do
  describe "NoTests" $ do
    it "Does not run integration tests if no token and chat id provided" $ do
      pending

runSpec [tkn,cId] = do
    let token = Token (T.pack tkn)
    let chatId = T.pack cId
    runSpec' token chatId

runSpec' :: Token -> Text -> SpecWith ()
runSpec' token chatId = do
    describe "Main" $ MainSpec.spec token chatId
    --describe "Inline" $ InlineSpec.spec token chatId