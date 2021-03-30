import Agents
import RunAgents
import Board

--import Control.Monad

main :: IO ()
main = do
    mv <- playAgent monteSimple startPosition
    print mv
