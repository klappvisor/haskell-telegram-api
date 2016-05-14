{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import           Control.Monad                (when)
import           Data.Maybe                   (fromMaybe)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified MainSpec
import           Options.Applicative
import           System.Environment           (getArgs, withArgs)
import           System.Exit                  (exitSuccess)
import           Test.Hspec
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Web.Telegram.API.Bot

-- | Command line options for test suite
data Options = Options
  {
    opt_token     :: String   -- ^ Bot token from BotFather
  , opt_chatId    :: String   -- ^ Id of a chat or of your bot
  , opt_botName   :: String   -- ^ Bot name
  , opt_hSpecOpts :: Maybe [String] -- ^ Command line options to pass to hSpec
  }

options :: Parser Options
options = Options
    <$> strOption
         ( long "token"
        <> short 't'
        <> metavar "BOT_TOKEN"
        <> help "Bot Token" )
    <*> strOption
         ( long "chatid"
        <> short 'c'
        <> metavar "CHAT_ID"
        <> help "Chat Id" )
    <*> strOption
         ( long "botname"
        <> short 'b'
        <> metavar "BOT_NAME"
        <> help "Bot Name" )
    <*> (optional (some (argument str
         ( metavar "HSPEC_ARGS"
        <> help "Hspec arguments"))))

main :: IO ()
main = do
    args <- getArgs
    -- If called with -t -b -c with no actual arguments
    -- don't test but return success.
    when (length args == 3) $ do
      putStrLn "Empty options, exiting with success"
      exitSuccess
    Options{..} <- execParser opts
    let token = Token ("bot" <> T.pack opt_token)
        chatId = T.pack opt_chatId
        botName = T.pack opt_botName
        hspecArgs = fromMaybe [] opt_hSpecOpts
    withArgs hspecArgs $ hspec (runSpec' token chatId botName)
    where opts = info (helper <*> options)
            ( fullDesc
           <> progDescDoc description)
runSpec' :: Token -> Text -> Text -> SpecWith ()
runSpec' token chatId botName =
    describe "Main" $ MainSpec.spec token chatId botName
    --describe "Inline" $ InlineSpec.spec token chatId botName

description ::  Maybe PP.Doc
description = Just $
           (PP.text  "Run the haskell-telegram-api tests")
    PP.<$> ((PP.text "Running with stack: ") PP.<> (PP.text "stack test --test-arguments=\"-t asd128903uiasbfì1023u -c 1235122 -b MyTeleBot -- -m send\""))
    PP.<$> ((PP.red . PP.text $ "WARNING: ") PP.<> (PP.text "the HSPEC_ARGS are optional but if present MUST be at the end and seperated from the other options with a -- "))
