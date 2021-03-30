{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Client where


import GHC.Generics

import qualified Network.WebSockets as WS
import Network.Socket (withSocketsDo)

import Control.Concurrent (forkIO)
import Control.Monad (forever, unless)
import Control.Monad.Trans (liftIO)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (fromStrict)

import Data.Aeson

import Data.Maybe (fromJust)

import Board
import Data.Array
import Data.List (transpose)

data Response = Response {
                    msg :: Text,
                    columns :: [[Int]],
                    width :: Int,
                    height :: Int
                } deriving Generic

instance FromJSON Response

parseBoard :: Text -> Position
parseBoard t = let responseByteString = fromStrict $ encodeUtf8 t
                   response = fromJust (decode responseByteString :: Maybe Response)
                   boardArray = array (0,120) $ zip [Coord c | c <- [0..120]] (map (toEnum . (`mod` 32) :: Int -> Piece) . concat . transpose . columns $ response)
                   agentPos = fst <$> find ((== A) . snd) $ assocs boardArray
                in undefined
                   
                   

testCli :: WS.ClientApp ()
testCli conn = do
    putStrLn "We're in"
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        let b = parseBoard msg
        liftIO $ T.putStrLn msg
    
    let loop = do
        line <- T.getLine
        print line
        unless (T.null line) $ WS.sendTextData conn (line <> (T.singleton '\n')) 
        putStrLn "sent stuff"
        loop
    loop
    WS.sendClose conn T.empty




main :: IO ()
main = withSocketsDo $ WS.runClient "automatafl.cslabs.clarkson.edu" 8080 "/" testCli
