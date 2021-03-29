import Agents
import Board

main :: IO ()
main = do
    mv1 <- playAgent simple startPosition
    print mv1
