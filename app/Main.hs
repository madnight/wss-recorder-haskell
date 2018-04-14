{-# LANGUAGE
   OverloadedStrings,
   ExtendedDefaultRules,
   TemplateHaskell,
   FlexibleInstances,
   TypeSynonymInstances
#-}

module Main where

import Lib

import Wuss
import Control.Monad (forever, void)
import Network.WebSockets (ClientApp, receiveData)
import Database.MongoDB
import Control.Monad.Trans (liftIO)
import System.Environment
import Network.URI
import Data.Maybe (fromJust)

main :: IO ()
main = do
   wssUrl <- getEnv "WSS_URL"
   let uri = fromJust $ parseAbsoluteURI wssUrl
   let path = uriPath uri ++ uriQuery uri
   let domain = uriRegName $ fromJust $ uriAuthority uri
   runSecureClient domain 443 path ws

ws :: ClientApp ()
ws connection = do
    pipe <- connect (host "127.0.0.1")
    void . forever $ do
        message <- receiveData connection
        let dataset = decodeBSON message
        d <- access pipe master "bitmex" (insert "subscription" dataset)
        print message
    close pipe
