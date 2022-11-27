{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Main (main) where

import           Data.Maybe                   (fromMaybe)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified JsonSpec
import qualified MainSpec
import           Options.Applicative
import qualified PaymentsSpec
import qualified StickersSpec
import           System.Environment           (lookupEnv, withArgs)
import           Test.Hspec
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.Read                    (readMaybe)
import qualified UpdatesSpec
import           Web.Telegram.API.Bot

-- | Command line options for test suite
data Options = Options
  {
    opt_integration :: Bool   -- ^ Run integration tests
  , opt_chatId      :: Maybe String   -- ^ Id of a chat or of your bot
  , opt_botName     :: Maybe String   -- ^ Bot name
  , opt_hSpecOpts   :: Maybe [String] -- ^ Command line options to pass to hSpec
  }

options :: Parser Options
options = Options
    <$> switch
        ( long "integration"
       <> help "Run integration tests" )
    <*> optional ( strOption
         ( long "chatid"
        <> short 'c'
        <> metavar "CHAT_ID"
        <> help "Chat Id" ))
    <*> optional ( strOption
         ( long "botname"
        <> short 'b'
        <> metavar "BOT_NAME"
        <> help "Bot Name" ))
    <*> optional ( some ( argument str
         ( metavar "HSPEC_ARGS"
        <> help "Hspec arguments")))

main :: IO ()
main = do
    tokenEnv <- lookupEnv "BOT_TOKEN"
    paymentTokenEnv <- lookupEnv "PAYMENT_TOKEN"
    Options{..} <- execParser opts
    let integration = opt_integration
        token = (\x -> Token ("bot" <> T.pack x)) <$> tokenEnv
        paymentToken = T.pack <$> paymentTokenEnv
        chatId = readChatId <$> opt_chatId
        botName = T.pack <$> opt_botName
        hspecArgs = fromMaybe [] opt_hSpecOpts
    withArgs hspecArgs $ hspec (runSpec' integration token chatId botName paymentToken)
    where opts = info (helper <*> options)
            ( fullDesc
           <> progDescDoc description)
runSpec' :: Bool -> Maybe Token -> Maybe ChatId -> Maybe Text -> Maybe Text -> SpecWith ()
runSpec' integration token chatId botName paymentToken = do
    describe "Unit tests" $
      describe "Json tests" JsonSpec.spec
    if integration then runIntegrationSpec token chatId botName paymentToken
    else describe "Integration tests" $ it "skipping..." $
        pendingWith "Use --integration switch to run integration tests"


runIntegrationSpec :: Maybe Token -> Maybe ChatId -> Maybe Text -> Maybe Text -> SpecWith ()
runIntegrationSpec (Just token) (Just chatId) (Just botName) (Just paymentToken) = do
        describe "Main integration tests" $ MainSpec.spec token chatId botName
        describe "Payments integration tests" $ PaymentsSpec.spec token chatId paymentToken
        describe "Updates API spec" $ UpdatesSpec.spec token botName
        describe "Stickers API spec" $ StickersSpec.spec token chatId
            --describe "Inline integration tests" $ InlineSpec.spec token chatId botName
runIntegrationSpec _ _ _ _ = describe "Integration tests" $
        error "Missing required arguments for integration tests. Run stack test --test-arguments \"--help\" for more info"

description ::  Maybe PP.Doc
description = Just $
           PP.text "Run the haskell-telegram-api tests"
    PP.<$> (PP.text "Running with stack: " PP.<> PP.text "stack test --test-arguments=\"--integration -c 1235122 -b MyTeleBot -- -m send\"")
    PP.<$> (PP.red (PP.text "WARNING: ") PP.<> PP.text "the HSPEC_ARGS are optional but if present MUST be at the end and seperated from the other options with a -- ")

readChatId :: String -> ChatId
readChatId s@('@':_) = ChatChannel $ T.pack s
readChatId (readMaybe -> Just s) = ChatId s
readChatId _ = error "ChatId must be either Integer or String in form '@channel'"
