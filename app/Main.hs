{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Telegram.API.Bots
import Servant
import Network.Wai
import Network.Wai.Handler.Warp

main :: IO ()
main = startApp

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users

users :: [User]
users = [ User 1 "Test" (Just "Test") Nothing
        , User 2 "TEST" (Just "TEST") Nothing
        ]
