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
                                          lblock  = if| null lblocks        -> None
                                                      | any (/= sr) lblocks -> Hard
                                                      | otherwise           -> Soft
                                          rblock = if| null rblocks         -> None
                                                      | any (/= sl) rblocks -> Hard
                                                      | otherwise           -> Soft
                                          both = [l,r]
                                          moves = case (lblock,rblock) of
                                              (None,None) -> both
                                              (None,Soft) -> both
                                              (None,Hard) -> [l]
                                              (Soft,None) -> both
                                              (Soft,Soft) -> both
                                              (Soft,Hard) -> []
                                              (Hard,None) -> [r]
                                              (Hard,_)    -> []
                                              in finish $ doMovesBlind moves pos


data Block = None | Soft | Hard

doMovesBlind :: [Move] -> Position -> Position
doMovesBlind ms pos@Position{posBoard=board} = pos{posBoard=board//concat [ [(s,V),(d,board!s)]  | (s,d) <- ms] }

finish :: Position -> Position
finish pos = doAgentStep pos{posConflicts=S.empty}


blocks :: Board -> Move -> [Coord]
blocks board mv = [ c | c <- spaces mv , board!c /= V ]

spaces :: Move -> [Coord]
spaces (s,d) = let
  (sx,sy) = toPair s
  (dx,dy) = toPair d
    in if | sx == dx -> let
            l = min sy dy
            r = max sy dy
              in [fromPair (sx,y) | y <- [l..r] ]
          | sy == dy -> let
            l = min sx dx
            r = max sx dx
              in [fromPair (x,sy) | x <- [l..r] ]
          | otherwise -> error "spaces called with illegal move"

doAgentStep :: Position -> Position
doAgentStep = undefined
