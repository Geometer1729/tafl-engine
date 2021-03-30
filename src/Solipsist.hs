module Solipsist where

import Board
import Rules
import Params
import Data.Array
import Eval
import Control.Monad.Reader
import Data.List
import Data.Ord
import Flow

directMoves :: Position -> [Move]
directMoves pos = do
   src <- [ coord | (coord,piece) <- assocs (posBoard pos) , piece `elem` [B,W] ]
   movesFrom pos src

movesFrom :: Position -> Coord -> [Move]
movesFrom pos src = do
    dir <- [up,right,down,left]
    genDir pos src dir

genDir :: Position -> Coord -> Coord -> [Move]
genDir pos src vec = let
  (_,d) = nearest (posBoard pos) src vec
    in [(src,src+i*vec) | i <- fromIntegral <$> [1..d-1] ]

doMove :: Move -> Position -> Position
doMove mv = doMovesBlind [mv]

solipCands :: Position -> WPs [Move]
solipCands pos = do
  candCount   <- asks solipCandAmount
  let cands = directMoves pos
  scores <- mapM (\mv -> positionEval $ doMove mv pos) cands
  let scoredCands = zip cands scores
  let sorted = map fst . sortOn (snd.>Down) $ scoredCands
  return $ take candCount sorted

