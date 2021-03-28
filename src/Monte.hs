module Monte
(analizePos)
where

import Board
import Rules
import Params
import System.Random
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader

type Agent = Position -> IO [(Float,Move)]
-- this should be moved to a more apropriate file once agents exist

analizePos :: Agent -> Position -> IOWP Float
analizePos ag pos = do
  games <- asks monteGames
  depth <- asks monteDepth
  lift $ score <$> runGames games depth pos ag

fromVal :: (Ord n,Num n)=> n -> [(n,a)] -> a
fromVal _ [] = error "val exceded sum of list probs"
fromVal v ((x,a):xs)
   | v < x     = a
   | otherwise = fromVal (v-x) xs

runOneGame :: Int -> Position -> Agent -> IO Result
runOneGame 0 _ _ = return Draw
runOneGame maxDepth pos ag = do
   val1 <- randomRIO (0,1) :: IO Float
   m1 <- ag pos
   val2 <- randomRIO (0,1) :: IO Float
   m2 <- ag (posFlip pos)
   let mv1  = fromVal val1 m1
   let mv2  = fromVal val2 m2
   let pos' = doMoves mv1 mv2 pos
   let mres  = checkWin pos'
   case mres of
      Nothing  -> runOneGame (maxDepth -1) pos' ag
      Just res -> return res

runGameMVar :: Int -> Position -> Agent -> IO (MVar Result)
runGameMVar depth pos ag = do
  mvar <- newEmptyMVar
  _ <- forkIO $ do
      res <- runOneGame depth pos ag
      putMVar mvar res
  return mvar

runGames :: Int -> Int -> Position -> Agent -> IO [Result]
runGames games depth pos ag = do
  mvars <- replicateM games (runGameMVar depth pos ag)
  mapM takeMVar mvars

scoreRes :: Result -> Float
scoreRes Win1 = 1
scoreRes Win2 = -1
scoreRes Draw = 0

score :: [Result] -> Float
score ress = sum (map scoreRes ress) / fromIntegral (length ress)

