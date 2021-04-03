{-# LANGUAGE TupleSections #-}
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
directMoves pos = let
  ms = directMoves' pos
                   in if null [ () | (src,_) <- ms , src == posAgent pos ]
                         then ms
                         else error "directMoves generated a bad move"

directMoves' :: Position -> [Move]
directMoves' pos = do
  src <- [ src | (src,piece) <- assocs (posBoard pos) , piece `elem` [B,W] , src /= posAgent pos ]
  movesFrom pos src

movesFrom :: Position -> Coord -> [Move]
movesFrom pos src = do
    dir <- [up,right,down,left]
    genDir pos src dir

genDir :: Position -> Coord -> Coord -> [Move]
genDir pos src vec = let
  board = posBoard pos
  dests = takeWhile (\dest -> board!dest == V)  $ do
      i <- [1..10]
      let dest = src+i*vec
      guard $ inRange (0,120) dest
      guard $ getX src + getX (i*vec) == getX dest
      return dest
  in map (src,) dests



  {-
genDir :: Position -> Coord -> Coord -> [Move]
genDir pos src vec = let
  (_,d) = nearest (posBoard pos) src vec
    in [(src,src+i*vec) | i <- fromIntegral <$> [1..d-1] ]
    -}

doMove :: Move -> Position -> Position
doMove mv pos = let
  pos' = doMovesBlind [mv] pos
                         in if not (validateAgent pos)
                           then error "doMove given bad agent"
                           else if not (validateAgent pos')
                               then error $ "doMove broke the agent\n" ++ show  mv ++ "\n" ++ showBoard pos ++ "\n" ++ show (posAgent pos)
                               else pos'

--doMove :: Move -> Position -> Position
--doMove mv = doMovesBlind [mv]

solipCands :: Position -> WPs [Move]
solipCands pos = if not (validateAgent pos)
                    then error "solip cands given bad agent"
                    else do
  candCount   <- asks solipCandAmount
  let cands = directMoves pos
  scores <- mapM (\mv -> positionEval $ doMove mv pos) cands
  let scoredCands = zip cands scores
  let sorted = map fst . sortOn (snd.>Down) $ scoredCands
  return $ take candCount sorted

