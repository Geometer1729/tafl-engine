module RunAgents where

import Board
import System.Random
import Control.Monad.Reader
import Control.Monad.State
import Params

playAgent :: Agent -> Position -> IO Move
playAgent ag pos = do
  g <- getStdGen
  let (dist,_) = runReader (runStateT (ag pos) g) def
  ix   <- randomRIO (0,1)
  let mv = fromVal ix dist
  return mv


fromVal :: (Ord n,Num n)=> n -> [(n,a)] -> a
fromVal _ [] = error "val called on empty list"
fromVal _ [(_,a)] = a -- kinda regretable but needed case because of floating point errors
fromVal v ((x,a):xs)
   | v < x     = a
   | otherwise = fromVal (v-x) xs

