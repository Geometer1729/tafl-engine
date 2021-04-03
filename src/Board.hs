{-# Language GeneralizedNewtypeDeriving #-}

module Board where

import Data.Array
import Data.Set
import Data.List
import Data.List.Split
import Data.Tuple
import Data.Bits
import Flow

newtype Coord = Coord Int deriving(Eq,Ord,Num,Ix,Enum,Real,Integral)

instance Show Coord where
  show = toPair.>show

data Piece = V | W | B | A deriving(Enum,Eq,Ord)

type Board = Array Coord Piece

data Result = Win1 | Win2 | Draw

data Position = Position{
  posBoard :: Board,
  posAgent :: Coord,
  posConflicts :: Set Coord
                  } deriving(Eq,Ord)

type Move = (Coord,Coord)

getX,getY :: Coord -> Int
getX = fst . toPair
getY = snd . toPair

toPair :: Coord -> (Int,Int)
toPair (Coord c) = swap $ quotRem c 11

fromPair :: (Int,Int) -> Coord
fromPair (x,y) = Coord $ y*11+x

instance Show Piece where
  show = return.showPiece

showPiece :: Piece -> Char
showPiece V = '.'
showPiece B = 'B'
showPiece W = 'W'
showPiece A = 'A'

showBoard :: Position -> String
showBoard pos = let board = Data.Array.elems $ posBoard pos
              in intercalate "\n" $ [[showPiece p | p <- row] | row <- chunksOf 11 board]

startBoard :: Array Coord Piece
startBoard = array (0,120) $ zip [Coord c | c <- [0..120]] [B,V,V,V,W,B,W,V,V,V,B,B,V,V,V,W,B,W,V,V,V,B,V,V,V,V,V,V,V,V,V,V,V,V,W,V,V,V,V,V,V,V,W,V,B,B,V,V,V,V,V,V,V,B,B,B,B,V,V,V,A,V,V,V,B,B,B,B,V,V,V,V,V,V,V,B,B,V,W,V,V,V,V,V,V,V,W,V,V,V,V,V,V,V,V,V,V,V,V,B,V,V,V,W,B,W,V,V,V,B,B,V,V,V,W,B,W,V,V,V,B] {-[
        B, B, V, V, B, B, B, V, V, B, B, --10
        V, V, V, W, B, B, B, W, V, V, V, --21
        V, V, V, V, V, V, V, V, V, V, V, --32
        V, V, V, V, V, V, V, V, V, V, V, --43
        W, W, V, V, V, V, V, V, V, W, W, --54
        B, B, V, V, V, A, V, V, V, B, B, --65
        W, W, V, V, V, V, V, V, V, W, W, --76
        V, V, V, V, V, V, V, V, V, V, V, --87
        V, V, V, V, V, V, V, V, V, V, V, --98
        V, V, V, W, B, B, B, W, V, V, V, --109
        B, B, V, V, B, B, B, V, V, B, B] --120
-}

startPosition :: Position
startPosition = Position{posBoard = startBoard, posAgent=60,posConflicts=empty}

instance Show Position where
    show = showBoard

posFlip :: Position -> Position
posFlip pos = let
  board = posBoard pos
  ag = posAgent pos
               in pos{posBoard=ixmap (0,120) (120 -) board,posAgent = 120-ag }

moveFlip :: Move -> Move
moveFlip (src,dest) = (120-src,120-dest)

validateAgent :: Position -> Bool
validateAgent Position{posBoard=board,posAgent=ag} = and [ (board!c /= A) `xor` (c == ag) | c <- [1..120] ]
