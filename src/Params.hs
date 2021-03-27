module Params where

import Control.Monad.Reader

type WPs = Reader Params

data Params = Params{
  epsilon :: Float,
  mwItters :: Int
                    }

def :: Params
def = Params{
  epsilon = 0.5,
  mwItters = 5
    }
