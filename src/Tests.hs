import Agents
import Board

import Control.Monad

main :: IO ()
main = do
    mvs <- replicateM 100 $ playAgent simple startPosition
    print mvs
