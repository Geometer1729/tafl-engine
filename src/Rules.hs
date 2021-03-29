{-# LANGUAGE MultiWayIf #-}

module Rules where

import Board
import Data.Bits
import Data.Array
import Data.Ord

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

checkWin :: Position -> Maybe Result
checkWin = undefined

nearest :: Board -> Coord -> Coord -> (Piece,Int)
nearest b c dir = let c' = c + dir
                   in if not $ inRange (0,120) c' && getX c + getX dir == getX c' -- detects wrap arounds which indicate board edges but don't leave range
                    then (V, -1)
                    else case b ! c' of
                            V -> let (t,d) = nearest b c' dir in (t,1+d)
                            W -> (W,1)
                            B -> (W,1)
                            A -> (A,1)

up,down,right,left :: Coord
up    = fromPair (0,1)
down  = fromPair (0,-1)
right = fromPair (1,0)
left  = fromPair (-1,0)

doAgentStep :: Position -> Position
doAgentStep p = let a = posAgent p
                    b = posBoard p
                    lookUp    = nearest b a up
                    lookDown  = nearest b a down
                    lookRight = nearest b a right
                    lookLeft  = nearest b a left
                    hPriority = genPriority lookLeft lookRight
                    vPriority = genPriority lookDown lookUp
                    vec = fromPriorities hPriority vPriority
                    move = (a,a+vec)
                      in doMovesBlind [move] p{posAgent=a+vec}


-- bool is true when the agent wants to move twoard the left argument
-- if this row/column takes priority
genPriority :: (Piece,Int) -> (Piece,Int) -> (Priority,Maybe Bool)
genPriority (pl,dl) (pr,dr) = let
    l = True
    r = False
                            in case (pl,pr) of
                                (V,V) -> (VV   ,Nothing )
                                (V,B) -> (BV dr,Just l)
                                (V,W) -> (WV dr,Just r)
                                (B,V) -> (BV dl,Just r)
                                (B,B) -> (BB (min dl dr) (max dl dr), case compare dl dr of
                                                                        LT -> Just r
                                                                        EQ -> Nothing
                                                                        GT -> Just l )
                                (B,W) -> (BW dl dr,Just r)
                                (W,V) -> (WV dl,Just l)
                                (W,B) -> (BW dr dl,Just l)
                                (W,W) -> (WW (min dl dr) (max dl dr), case compare dl dr of
                                                                        LT -> Just l
                                                                        EQ -> Nothing
                                                                        GT -> Just r )
                                (A,_) -> error "agent found agent"
                                (_,A) -> error "agent found agent"


fromPriorities :: (Priority,Maybe Bool) -> (Priority,Maybe Bool) -> Coord
fromPriorities (ph,mhd) (pv,mvd) = case (mhd,mvd) of
                                     (Nothing,Nothing) -> 0
                                     (Just h ,Nothing) -> if h then left else right
                                     (Nothing,Just  v) -> if v then down else up
                                     (Just h ,Just  v) -> let
                                                      hv = if h then left else right
                                                      vv = if v then down else up
                                                        in case compare ph pv of
                                                              LT -> hv
                                                              EQ -> vv --column rule
                                                              GT -> vv


data Priority =
    BW Int Int
  | BV Int
  | BB Int Int
  | WV Int
  | WW Int Int
  | VV
    deriving(Read,Show,Eq)

-- high priortiy < low priority
-- makes the instance simpler by avoiding flips as low numbers corespond to higher priority
instance Ord Priority where
    compare (BW bl wl) (BW br wr) = compare (wl,bl) (wr,br)
    compare (BW _ _ )  _          = LT
    compare (BV l)     (BV r)     = compare l r
    compare (BV l)     (BB rc rf) = compare (l,Down 12) (rc,Down rf) -- 12 is basically infinite
    compare (BV _)     _          = LT
    compare (BB lc lf) (BB rc rf) = compare (lc,Down lf) (rc,Down rf)
    compare (BB _ _)   _          = LT
    compare (WV l)     (WV r)     = compare l r
    compare (WV l)     (WW rc rf) = compare (l,Down 12)  (rc,Down rf)
    compare (WV _)     _          = LT
    compare (WW lc lf) (WW rc rf) = compare (lc,Down lf) (rc,Down rf)
    compare (WW _ _)   _          = LT
    compare VV         VV         = EQ
    compare l r = compare (Down r) (Down l)

