module Agents where

import Board
import Params
import Solipsist
import ResponseSimple
import Simul
import Eval
import Rules
import Monte

import Control.Monad
import Control.Parallel.Strategies
import Control.Monad.Reader
import Data.Array
import System.Random
import Control.Monad.State

rwpPar :: [RWP a ] -> RWP [a]
rwpPar xs = StateT $ \g -> reader $ \e -> let
  (g1,g2) = split g
  gens = map mkStdGen (randoms g1)
  ys = [ fst $ runReader (runStateT x g') e | (x,g') <- zip xs gens ] `using` parList rpar
    in (ys,g2)

withM :: MonadReader r m => Reader r a -> m a
withM = reader.runReader

simpleCands :: Position -> RWP ([Move],[Move])
simpleCands pos = do
  solip1 <- withM $ solipCands pos
  solip2 <- withM $ map moveFlip <$> solipCands (posFlip pos)
  res1   <- withM $ concat <$> forM solip2 (simpleResponses pos)
  res2   <- withM $ map moveFlip . concat <$> forM solip1 (simpleResponses (posFlip pos))
  let ms1 = solip1 ++ res1
      ms2 = solip2 ++ res2
  return (ms1,ms2)

simple :: Agent
simple pos = do
  (ms1,ms2) <- simpleCands pos
  evals <- withM $ mapM (fmap fromIntegral.positionEval) [ doMoves m1 m2 pos | m1 <- ms1 , m2 <- ms2 ]
  let game = listArray ((0,0),(length ms1-1,length ms2-1)) evals
  (d1,_) <- withM $ solveGame game
  return $ zip (elems d1) ms1

monteSimple :: Agent
monteSimple pos = do
  (ms1,ms2) <- simpleCands pos
  evals <- rwpPar [ analizePos simple $ doMoves m1 m2 pos | m1 <- ms1 , m2 <- ms2 ]
  let game = listArray ((0,0),(length ms1-1,length ms2-1)) evals
  (d1,_) <- withM $ solveGame game
  return $ zip (elems d1) ms1

