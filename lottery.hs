import System.Random (randomRIO)
import Data.Char
mylist = getLine
randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
  r  <- randomRIO (1,31)
  rs <- randomList (n-1)
  return (r:rs) 
comp mylist = randomList n == mylist