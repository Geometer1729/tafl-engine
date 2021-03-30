module Monte
(analizePos)
where

import Board
import Rules
import Params
import RunAgents
import Eval

import System.Random
import Control.Monad.Reader
import Control.Monad.State


analizePos :: Agent -> Position -> RWP Float
analizePos ag pos = do
  games <- asks monteGames
  depth <- asks monteDepth
  sum <$> runGames games depth pos ag

runOneGame :: Int -> Position -> Agent -> RWP Float
runOneGame 0 pos _ = reader . runReader $ fromIntegral <$> positionEval pos
runOneGame maxDepth pos ag = do
   val1 <- state $ randomR (0,1)
   m1 <- ag pos
   val2 <- state $ randomR (0,1)
   m2 <- ag (posFlip pos)
   let mv1  = fromVal val1 m1
   let mv2  = fromVal val2 m2
   let pos' = doMoves mv1 mv2 pos
   let mres  = checkWin pos'
   case mres of
      Nothing  -> runOneGame (maxDepth -1) pos' ag
      Just res -> scoreRes pos res

runGames :: Int -> Int -> Position -> Agent -> RWP [Float]
runGames games depth pos ag = replicateM games (runOneGame depth pos ag)

scoreRes :: Position -> Result -> RWP Float
scoreRes _ Win1 = asks winEval
scoreRes _ Win2 = asks (negate.winEval)
scoreRes pos Draw = lift $ fromIntegral <$> positionEval pos

