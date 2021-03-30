module Client where

{-# LANGUAGE OverloadedStrings #-}

import qualified Network.WebSockets as WS
import Network.Socket (withSocketsDo)

import Control.Concurrent (forkIO)
import Control.Monad (forever, unless)
import Control.Monad.Trans (liftIO)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

testCli :: WS.ClientApp ()
testCli conn = do
    putStrLn "We're in"
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn msg
    
    let loop = do
        line <- T.getLine
        unless (T.null line) $ WS.sendTextData conn line >> loop
    loop
    WS.sendClose conn T.empty


main :: IO ()
main = WS.runClient "automatafl.cslabs.clarkson.edu" 8080 "/" testCli
