{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Board where

import Data.Array
import Data.Set

newtype Coord = Coord Int deriving(Eq,Ord,Num,Ix)

data Piece = V | B | W | A deriving(Enum,Eq,Ord)

type Board = Array Coord Piece

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

