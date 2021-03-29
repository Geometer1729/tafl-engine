module Monte
(analizePos)
where

import Board
import Rules
import Params
import Agents

import System.Random
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader


analizePos :: Agent -> Position -> IOWP Float
analizePos ag pos = do
  games <- asks monteGames
  depth <- asks monteDepth
  score <$> runGames games depth pos ag

runOneGame :: Int -> Position -> Agent -> IOWP Result
runOneGame 0 _ _ = return Draw
runOneGame maxDepth pos ag = do
   val1 <- lift $ randomRIO (0,1)
   m1 <- ag pos
   val2 <- lift $ randomRIO (0,1)
   m2 <- ag (posFlip pos)
   let mv1  = fromVal val1 m1
   let mv2  = fromVal val2 m2
   let pos' = doMoves mv1 mv2 pos
   let mres  = checkWin pos'
   case mres of
      Nothing  -> runOneGame (maxDepth -1) pos' ag
      Just res -> return res

runGameMVar :: Int -> Position -> Agent -> IOWP (MVar Result)
runGameMVar depth pos ag = do
  mvar <- lift newEmptyMVar
  e <- ask
  _ <- lift $ forkIO $ do
      res <- runReaderT (runOneGame depth pos ag) e
      putMVar mvar res
  return mvar

runGames :: Int -> Int -> Position -> Agent -> IOWP [Result]
runGames games depth pos ag = do
  mvars <- replicateM games (runGameMVar depth pos ag)
  lift $ mapM takeMVar mvars

scoreRes :: Result -> Float
scoreRes Win1 = 1
scoreRes Win2 = -1
scoreRes Draw = 0

score :: [Result] -> Float
score ress = sum (map scoreRes ress) / fromIntegral (length ress)

