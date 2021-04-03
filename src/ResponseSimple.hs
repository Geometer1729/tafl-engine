module ResponseSimple where

import Params
import Board
import Rules
import Solipsist
import Eval

import Control.Monad.Reader
import Data.Ord
import Data.List
import Flow

simpleResponses :: Position -> Move -> WPs [Move]
simpleResponses pos move@(_,dest) = do
   taking <- asks simpleResCount
   let direct = directMoves pos
       tran   = movesFrom (doMove move pos) dest
       moves = direct ++ tran
   scores <- forM moves (\mv -> positionEval (doMoves mv move pos))
   zip moves scores |> sortOn (snd.>Down) .> map fst .> take taking .> return

