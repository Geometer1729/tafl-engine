{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Board where

import Data.Array
import Data.Set
import Data.List
import Data.List.Split

newtype Coord = Coord Int deriving(Eq,Ord,Num,Ix)

data Piece = V | B | W | A deriving(Enum)

data Position = Position{
  posBoard :: Array Coord Piece,
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

showPiece :: Piece -> Char
showPiece V = '.'
showPiece B = 'B'
showPiece W = 'W'
showPiece A = 'A'

showBoard :: Position -> String
showBoard p = let board = Data.Array.elems $ posBoard p
              in intercalate "\n" $ [[showPiece p | p <- row] | row <- chunksOf 11 board]

startBoard :: Array Coord Piece 
startBoard = array (0,120) $ zip [Coord c | c <- [0..120]] [
        B, B, V, V, B, B, B, V, V, B, B,
        V, V, V, W, B, B, B, W, V, V, V,
        V, V, V, V, V, V, V, V, V, V, V,
        V, V, V, V, V, V, V, V, V, V, V,
        W, W, V, V, V, V, V, V, V, W, W,
        B, B, V, V, V, A, V, V, V, B, B,
        W, W, V, V, V, V, V, V, V, W, W,
        V, V, V, V, V, V, V, V, V, V, V,
        V, V, V, V, V, V, V, V, V, V, V,
        V, V, V, W, B, B, B, W, V, V, V,
        B, B, V, V, B, B, B, V, V, B, B]

startPosition :: Position
startPosition = Position{posBoard = startBoard, posAgent=55,posConflicts=empty}

instance Show Position where
    show = showBoard
