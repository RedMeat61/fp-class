import Control.Monad.State
import System.Environment

type Queue = [Int]

enqueue :: Int -> State Queue ()
enqueue x = do
  xs <- get
  put (xs ++ [x])

dequeue :: State Queue Int
dequeue = do
  (x:xs) <- get
  put xs
  return x