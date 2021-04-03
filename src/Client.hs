{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Client where

import GHC.Generics

import qualified Network.WebSockets as WS
import Network.Socket (withSocketsDo)

import Control.Concurrent
--import Control.Monad (forever, unless)
--import Control.Monad.Trans (liftIO)
import Control.Monad.State

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (fromStrict)

import Data.Aeson

import Data.Maybe (fromJust)
import Data.Bits

import Board
import RunAgents
import Agents
import Data.Array
import Data.List (transpose,find)
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S


data StateMsg = StateMsg {
                    msg :: Text,
                    columns :: [[Int]],
                    width :: Int,
                    height :: Int
                } deriving Generic

instance FromJSON StateMsg

data NewTurn = NewTurn

parseMsg :: Text -> Maybe (Either Position NewTurn)
parseMsg t = let
    responseByteString = fromStrict $ encodeUtf8 t
    maybeStateMsg = decode responseByteString :: Maybe StateMsg
      in case maybeStateMsg of
           Just stateMsg -> let
              intArray = listArray (0,120) (concat . transpose . columns $ stateMsg)
              boardArray = fmap (toEnum . (`mod` 16) :: Int -> Piece) intArray
              conflicts =  S.fromList [ c | (c,x) <- assocs intArray , x .|. 0x10 /= 0 ]
              agentPos = fst $ fromJust $ find ((== A) . snd) $ assocs boardArray
              pos = Position{posBoard=boardArray,posAgent=agentPos,posConflicts=conflicts}
                in if validateAgent pos
                      then Just $ Left pos
                      else error "invalid agent in parseMsg"
           Nothing -> do
              jsonMap <- decode responseByteString :: Maybe Object
              k <- H.lookup "kind" jsonMap
              case k of
                  String "TURN_OVER" -> Just (Right NewTurn)
                  String "CONFLICT"  -> Just (Right NewTurn)
                  _ -> Nothing



type ClientState = (Position,MVar Position)
type CIO = StateT ClientState IO

handleMessage :: Either Position NewTurn -> CIO ()
handleMessage (Left posNew) = do
  (posOld,mvar) <- get
  if posBoard posOld /= posBoard posNew -- this doesn't quite work for switch moves
     then do
       put (posNew,mvar)
       lift $ putMVar mvar posOld
     else put (posNew,mvar)
handleMessage (Right NewTurn) = do
  (pos,mvar) <- get
  lift $ putMVar mvar pos


testCli :: WS.ClientApp ()
testCli conn = do
    posMVar <- newEmptyMVar :: IO (MVar Position)
    putStrLn "We're in"
    _ <- forkIO $ void $ flip runStateT (startPosition,posMVar) $ forever $ do
        msg' <- lift $ WS.receiveData conn
        let maybeMsg = parseMsg msg'
        forM_ maybeMsg handleMessage -- yeah I fold maybes
        liftIO $ T.putStrLn msg'
    WS.sendTextData conn ("set_name engine\n" :: Text )
    WS.sendTextData conn ("be_p 1\n" :: Text )
    putMVar posMVar startPosition
    _ <- forkIO $ void $ forever $ do
        pos <- takeMVar posMVar
        print ("calling agent" :: String)
        (src,dest) <- playAgent monteSimple pos
        let (x1,y1) = toPair src
            (x2,y2) = toPair dest
        WS.sendTextData conn ("move " <> T.pack (unwords (map show [x1,y1,x2,y2])) <> "\n")
    _ <- forever $ do
        line <- T.getLine
        print line
        unless (T.null line) $ WS.sendTextData conn (line <> T.singleton '\n')
        putStrLn "sent stuff"
    WS.sendClose conn T.empty

client :: IO ()
client = withSocketsDo $ WS.runClient "automatafl.cslabs.clarkson.edu" 8080 "/" testCli
