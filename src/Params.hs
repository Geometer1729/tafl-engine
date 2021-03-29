module Params where

import Control.Monad.Reader

type IOWP = ReaderT Params IO
type WPs  = Reader Params

data Params = Params{
  epsilon :: Float,
  mwItters :: Int,
  monteGames :: Int,
  monteDepth :: Int,
  agentWeights :: [Int],
  evalWeights :: [Int],
  solipCandAmount :: Int,
  simpleResCount :: Int
                    }

def :: Params
def = Params{
  epsilon = 0.5,
  mwItters = 5,
  monteGames = 100,
  monteDepth = 200,
  agentWeights = [5,4,3,2,1],
  evalWeights  = [1,1,1],
  solipCandAmount = 10,
  simpleResCount = 3
    }
