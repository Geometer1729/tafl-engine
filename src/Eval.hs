module Eval where

import Board
import Rules
import Params
import Data.Array
import Control.Monad.Reader

{-
newtype Coord = Coord Int deriving(Eq,Ord,Num,Ix)

data Piece = V | B | W | A deriving(Enum)

data Position = Position{
  posBoard :: Array Coord Piece,
  posAgent :: Coord,
  posConflicts :: Set Coord
                  }

type Move = (Coord,Coord)
-}

l1 :: Coord -> Coord -> Int
l1 c1 c2 = let
  (x1,y1) = toPair c1
  (x2,y2) = toPair c2
    in abs (x1-x2) + abs (y1-y2)

positionEval :: Position -> WPs Int -- Weights
positionEval p = do
  ws <- asks evalWeights
  ae <- agentEval p
  if validateAgent p
    then return $ sum $ zipWith (*) ws [ae,repulsorEval p,attractorEval p]
    else error "position eval called with broken agent"

agentEval :: Position -> WPs Int
agentEval p = do
  ws <- asks agentWeights
  return $ sum $ zipWith (*) ws (map agentEval' $ iterate doAgentStep p)

agentEval' :: Position -> Int
agentEval' p = let loc = posAgent p
--              in if loc < 55 then 10 - min (l1 (Coord 0) loc) (l1 (Coord 10) loc)
--              else if loc < 66 then 0
--              else -10 + min (l1 (Coord 110) loc) (l1 (Coord 120) loc)
              in heatmap ! loc

  {-
    it would probably be better if repulsor and atractor eval
    looked at the position of the agent more
    ie repulsors are bad if they are between you and the nearest corner
    attractors are good between you and the corner unless you're really close
    etc
    -}

repulsorEval :: Position -> Int
repulsorEval p = let board = posBoard p
                     idx = indices board
                     accumulateRepulsors :: Coord -> Int -> Int
                     accumulateRepulsors c t = case board ! c of
                                                  B -> t - (heatmap ! c)
                                                  _ -> t
                in foldr accumulateRepulsors 0 idx

attractorEval :: Position -> Int
attractorEval p = let board = posBoard p
                      idx = indices board
                      accumulateAttractors :: Coord -> Int -> Int
                      accumulateAttractors c t = case board ! c of
                                                  W -> t + (heatmap ! c)
                                                  _ -> t
                  in foldr accumulateAttractors 0 idx

debugProduceHeatmap :: [Int]
debugProduceHeatmap = [agentEval' startPosition{posAgent=Coord i} | i <- [0..120]]

heatmap :: Array Coord Int
heatmap = array (0,120) $ zip [Coord c | c <- [0..120]] [10,9,8,7,6,5,6,7,8,9,10,9,8,7,6,5,4,5,6,7,8,9,8,7,6,5,4,3,4,5,6,7,8,7,6,5,4,3,2,3,4,5,6,7,6,5,4,3,2,1,2,3,4,5,6,0,0,0,0,0,0,0,0,0,0,0,-6,-5,-4,-3,-2,-1,-2,-3,-4,-5,-6,-7,-6,-5,-4,-3,-2,-3,-4,-5,-6,-7,-8,-7,-6,-5,-4,-3,-4,-5,-6,-7,-8,-9,-8,-7,-6,-5,-4,-5,-6,-7,-8,-9,-10,-9,-8,-7,-6,-5,-6,-7,-8,-9,-10]
