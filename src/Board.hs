{-# Language GeneralizedNewtypeDeriving #-}

module Board where

import Data.Array
import Data.Set
import Data.List
import Data.List.Split
import Flow

newtype Coord = Coord Int deriving(Eq,Ord,Num,Ix)

instance Show Coord where
  show = toPair.>show

data Piece = V | B | W | A deriving(Enum,Eq,Ord)

type Board = Array Coord Piece

data Result = Win1 | Win2 | Draw

data Position = Position{
  posBoard :: Board,
  posAgent :: Coord,
  posConflicts :: Set Coord
                  }

type Move = (Coord,Coord)

getX,getY :: Coord -> Int
getX (Coord val) = val `mod` 11
getY (Coord val) = val `div` 11

toPair :: Coord -> (Int,Int)
toPair coord = (getX coord,getY coord)

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
    in pos{posBoard=ixmap (0,120) (120 -) board}

moveFlip :: Move -> Move
moveFlip (src,dest) = (120-src,120-dest)

