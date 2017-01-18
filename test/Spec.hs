{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified JsonSpec
import qualified MainSpec
import           Options.Applicative
import qualified SettingsSpec
import           System.Environment           (withArgs)
import           Test.Hspec
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Web.Telegram.API.Bot

-- | Command line options for test suite
data Options = Options
  {
    opt_integration :: Bool   -- ^ Run integration tests
  , opt_token       :: Maybe String   -- ^ Bot token from BotFather
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
         ( long "token"
        <> short 't'
        <> metavar "BOT_TOKEN"
        <> help "Bot Token" ))
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
    Options{..} <- execParser opts
    let integration = opt_integration
        token = fmap (\x -> Token ("bot" <> T.pack x)) opt_token
        chatId = T.pack <$> opt_chatId
        botName = T.pack <$> opt_botName
        hspecArgs = fromMaybe [] opt_hSpecOpts
    withArgs hspecArgs $ hspec (runSpec' integration token chatId botName)
    where opts = info (helper <*> options)
            ( fullDesc
           <> progDescDoc description)
runSpec' :: Bool -> Maybe Token -> Maybe Text -> Maybe Text -> SpecWith ()
runSpec' integration token chatId botName = do
    describe "Unit tests" $ do
      describe "Json tests" JsonSpec.spec
    if integration then runIntegrationSpec token chatId botName
    else describe "Integration tests" $ it "skipping..." $
        pendingWith "Use --integration switch to run integration tests"


runIntegrationSpec :: Maybe Token -> Maybe Text -> Maybe Text -> SpecWith ()
runIntegrationSpec (Just token) (Just chatId) (Just botName) = do
        describe "Main integration tests" $ MainSpec.spec token chatId botName
        describe "Settings API spec" $ SettingsSpec.spec token botName
            --describe "Inline integration tests" $ InlineSpec.spec token chatId botName
runIntegrationSpec _ _ _ = describe "Integration tests" $ do
        fail "Missing required arguments for integration tests. Run stack test --test-arguments \"--help\" for more info"

description ::  Maybe PP.Doc
description = Just $
           (PP.text "Run the haskell-telegram-api tests")
    PP.<$> ((PP.text "Running with stack: ") PP.<> (PP.text "stack test --test-arguments=\"--integration -t asd128903uiasbfì1023u -c 1235122 -b MyTeleBot -- -m send\""))
    PP.<$> ((PP.red . PP.text $ "WARNING: ") PP.<> (PP.text "the HSPEC_ARGS are optional but if present MUST be at the end and seperated from the other options with a -- "))
