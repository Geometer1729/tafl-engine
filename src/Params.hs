module Params where

import Board

import Control.Monad.Reader
import Control.Monad.State
import System.Random

type IOWP = ReaderT Params IO
type RWP = StateT StdGen (Reader Params)
type WPs  = Reader Params

type Agent = Position -> RWP [(Float,Move)]

data Params = Params{
  epsilon :: Float,
  mwItters :: Int,
  monteGames :: Int,
  monteDepth :: Int,
  agentWeights :: [Int],
  evalWeights :: [Int],
  solipCandAmount :: Int,
  simpleResCount :: Int,
  winEval :: Float
                    }

def :: Params
def = Params{
  epsilon = 0.1,
  mwItters = 15,
  monteGames = 10,
  monteDepth = 5,
  agentWeights = [5,4,3,2,1],
  evalWeights  = [1,1,1],
  solipCandAmount = 4,
  simpleResCount = 3,
  winEval = 100
    }
