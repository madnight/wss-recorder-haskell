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

main :: IO ()
main = runSecureClient "www.bitmex.com" 443 "/realtime?subscribe=orderBookL2:XBTUSD,quote:XBTUSD,trade:XBTUSD" ws

ws :: ClientApp ()
ws connection = do
    pipe <- connect (host "127.0.0.1")
    void . forever $ do
        message <- receiveData connection
        let dataset = decodeBSON message
        d <- access pipe master "bitmex" (insert "subscription" dataset)
        print message
    close pipe
