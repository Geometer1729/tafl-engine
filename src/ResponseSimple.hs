module ResponseSimple where

import Params
import Board
import Rules
import Solipsist
import Eval

import Control.Monad.Reader
import Data.List
import Data.Ord
import Flow

simpleResponses :: Position -> Move -> WPs [Move]
simpleResponses pos move = do
   taking <- asks simpleResCount
   let moves =  directMoves (doMove move pos) `union` directMoves pos
   scores <- forM moves (\mv -> positionEval (doMoves mv move pos))
   zip moves scores |> sortOn (snd.>Down) .> map fst .> take taking .> return

