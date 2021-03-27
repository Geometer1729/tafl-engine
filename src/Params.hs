module Params where

import Control.Monad.Reader

type WPs = Reader Params

data Params = Params{
  epsilon :: Float,
  mwItters :: Int
                    }

