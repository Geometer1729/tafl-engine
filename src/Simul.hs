module Simul where

import Data.Array
import Params
import Control.Monad.Reader

type Game = Mat Float
type Dist = Vec Float

type Mat = Array (Int,Int)
type Vec = Array Int

-- this will probably comprise a good deal of the computation
-- so ideally it should get graphics card acceleration eventually.

lMult :: Num a => Vec a -> Mat a -> Vec a
lMult v m = let
  (_,(a,b)) = bounds m
    in listArray (0,b) [ sum [ m!(i,j)*v!j | j <- [0..a]] | i <- [0..a] ]

rMult :: Num a => Mat a -> Vec a -> Vec a
rMult m v = let
  (_,(a,b)) = bounds m
    in listArray (0,a) [ sum [ m!(i,j)*v!j | j <- [0..b]]   | i <- [0..a] ]

-- game -> optimal dist for 1,2
solveGame :: Game -> WPs (Dist,Dist)
solveGame g = do
  n <- asks mwItters
  let (_,(a,b)) = bounds g
  foldM (flip mwStep) (listArray (0,a) (repeat 1),listArray (0,b) (repeat 1)) (replicate n g)


mwStep :: Game -> (Dist,Dist) -> WPs (Dist,Dist)
mwStep g (p1,p2) = asks epsilon >>= \ eps -> let
  ct1 = g  `rMult` p2
  ct2 = p1 `lMult` g
  b1 = bounds p1
  b2 = bounds p2
  -- playr 1 maximizes so the costs are negative for p1 and positive for p2
  p1' = normalize $ listArray b1 [ p1!i * (1-eps)**(- ct1!i) | i <- range b1 ]
  p2' = normalize $ listArray b2 [ p2!i * (1-eps)**(- ct2!i) | i <- range b2 ]
    in return (p1',p2')

-- ideally normalizing would be done somewhat less often

normalize :: Dist -> Dist
normalize v = let
  s = sum v
    in fmap (/s) v

-- test game
soccer :: Game
soccer = listArray ((0,0),(1,1)) [0,2,1,0]

soccerSol :: (Dist,Dist)
soccerSol = runReader (solveGame soccer) def


