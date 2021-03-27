{-# LANGUAGE MultiWayIf #-}

module Rules where

import Board
import Data.Bits
import Data.Array

import qualified Data.Set as S

isLegal :: Position -> Move -> Bool
isLegal Position{posAgent=agent,posConflicts=conflicts} mv@(src,dest) = and [
   nonDiagonal mv,
   src /= agent,
   src /= dest,
   inRange (0,120) src,
   inRange (0,120) dest,
   src  `notElem` conflicts,
   dest `notElem` conflicts
    ]

nonDiagonal :: Move -> Bool
nonDiagonal (src,dest) = let
  (sx,sy) = toPair src
  (dx,dy) = toPair dest
    in (sx == dx) `xor` (sy == dy)

findConflict :: Move -> Move -> Maybe Coord
findConflict m1@(s1,d1) m2@(s2,d2)
  | m1 == m2  = Nothing
  | s1 == s2  = Just s1
  | d1 == d2  = Just d1
  | otherwise = Nothing

checkTransitive :: Move -> Move -> Maybe (Coord,Coord,Coord)
checkTransitive (s1,d1) (s2,d2)
  | d1 == s2  = Just (s1,d1,d2)
  | d2 == s1  = Just (s2,d2,d1)
  | otherwise = Nothing

occupied :: Board -> Coord -> Bool
occupied board coord = board!coord /= V

doMoves :: Move -> Move -> Position -> Position
doMoves mv1 mv2 pos@Position{posBoard=board} = case findConflict mv1 mv2 of
                        Just conf -> pos{posConflicts= S.insert conf (posConflicts pos)}
                        Nothing   -> case checkTransitive mv1 mv2 of
                                       Just (s,m,d) -> let
                                          ocp = occupied board
                                          ocs = (ocp s,ocp m,ocp d)
                                          lblocked = any  (/=m) $ blocks board (s,m)
                                          rblocked = not . null $ blocks board (m,d)
                                          blocked = (lblocked,rblocked)
                                          l = (s,m)
                                          r = (m,d)
                                          f = (s,d)
                                          -- this can probably be simplified considerably
                                          moves = case ocs of
                                                    (_,True,True)                         -> [ ]
                                                    (True,True,False) -> case blocked of
                                                                           (_,True)       -> [ ]
                                                                           (_,False)      -> [r]
                                                    (True,False,True)  -> case blocked of
                                                                            (True,_)      -> [ ]
                                                                            (False,_)     -> [l]
                                                    (True,False,False) -> case blocked of
                                                                            (True,_)      -> [ ]
                                                                            (False ,True) -> [l]
                                                                            (False,False) -> [f]
                                                    (False,True,False) -> case blocked of
                                                                            (_,True)      -> [ ]
                                                                            (_,False)     -> [r]
                                                    (False,False,_)                       -> [ ]
                                        in finish . doMovesBlind moves $ pos
                                       Nothing -> let
                                          l = mv1
                                          r = mv2
                                          (sl,_) = l
                                          (sr,_) = r
                                          lblocks = blocks board l
                                          rblocks = blocks board r
                                          lblocked  = not . null $ lblocks
                                          lblocked' = any (/= sr)  lblocks
                                          rblocked  = not . null $ rblocks
                                          rblocked' = any (/= sl)  rblocks
                                          moves = case (lblocked,lblocked',rblocked,rblocked') of
                                                (True,True,True,True)   -> []
                                                (True,True,True,False)  -> undefined
                                                _ -> undefined
                                              in finish $ doMovesBlind moves pos


doMovesBlind :: [Move] -> Position -> Position
doMovesBlind = undefined

finish :: Position -> Position
finish pos = doAgentStep pos{posConflicts=S.empty}


blocks :: Board -> Move -> [Coord]
blocks = undefined

spaces :: Move -> [Coord]
spaces = undefined

nearest :: Board -> Coord -> Axis -> (Piece,Int)
nearest b (Coord c) a = let newidx = Coord $ c + fromPair a
                in if not inRange (0,120) newidx
                    then (V, -1)
                    else case b ! newidx of
                            V -> 1 + nearest b newidx a
                            W -> (W,1)
                            B -> (W,1)
                            A -> (A,1)

doAgentStep :: Position -> Position
doAgentStep p = let a = posAgent p
                    b = posBoard p
                    colUp = nearest b a (1,0)
                    colDown = nearest b a (-1,0)
                    rowUp = nearest b a (0,1)
                    rowDown = nearest b a (0,-1)
                    --find a repulsor
                    if | fst colUp == B && fst colDown == W && snd colDown != 1 -> doMovesBlind [(a,a-11)] p
                       | fst colDown == B && fstColUp == W && snd colUp != 1 -> doMovesBlind [(a,a+11)] p
                       | 
