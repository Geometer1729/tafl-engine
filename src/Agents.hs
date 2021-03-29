module Agents where

import Board
import Params
import Solipsist
import ResponseSimple
import Simul
import Eval
import Rules

import Control.Monad
import Control.Monad.Reader
import Data.Array
import System.Random

type Agent = Position -> IOWP  [(Float,Move)]

withM :: MonadReader r m => Reader r a -> m a
withM = reader.runReader

playAgent :: Agent -> Position -> IO Move
playAgent ag pos = do
  dist <- runReaderT (ag pos) def
  ix   <- randomRIO (0,1)
  let mv = fromVal ix dist
  return mv

fromVal :: (Ord n,Num n)=> n -> [(n,a)] -> a
fromVal _ [] = error "val exceded sum of list probs"
fromVal v ((x,a):xs)
   | v < x     = a
   | otherwise = fromVal (v-x) xs

test :: Agent
test pos =  do
  solip1 <- withM $ solipCands pos
  solip2 <- withM $ solipCands (posFlip pos)
  lift $ putStrLn "solip1"
  mapM_ (lift.print) solip1
  lift $ putStrLn "solip2"
  mapM_ (lift.print) solip2
  evals  <- withM $ sequence [ fromIntegral <$> positionEval (doMoves m1 m2 pos) | m1 <- solip1 , m2 <- solip2 ]
  lift $ putStrLn "evals"
  mapM_ (lift.print) evals
  let game = listArray ((0,0),(length solip1-1,length solip2-1)) evals
  (d1,_) <- withM $ solveGame game
  return $ zip (elems d1) solip1


simple :: Agent
simple pos = do
  solip1 <- withM $ solipCands pos
  solip2 <- withM $ solipCands (posFlip pos)
  res1   <- withM $ concat <$> forM solip2 (simpleResponses pos)
  res2   <- withM $ concat <$> forM solip1 (simpleResponses pos)
  let ms1 = solip1 ++ res1
      ms2 = solip2 ++ res2
  evals <- withM $ sequence [ fromIntegral <$> positionEval (doMoves m1 m2 pos) | m1 <- ms1 , m2 <- ms2 ]
  let game = listArray ((0,0),(length ms1-1,length ms2-1)) evals
  (d1,_) <- withM $ solveGame game
  return $ zip (elems d1) ms1

