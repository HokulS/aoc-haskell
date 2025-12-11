import System.IO 
import Control.Monad 
import Data.List

main = do 
  contents <- readFile "puzzle.txt"
  let a = map readInt . words $ contents
  let first = evens a
  let second = odds a 

  print (similarity first second)




readInt :: String -> Int
readInt = read

evens (x:xs) = x : odds xs
evens [] = [] 

odds (_:xs) = evens xs
odds [] = []

occurences :: [Int] -> Int -> Int 
occurences [] _ = 0
occurences (x:xs) y 
  | x == y = 1 + occurences xs y 
  | otherwise = occurences xs y


similarity :: [Int] -> [Int] -> Int 
similarity [] _ = 0
similarity (x:xs) y = x * (occurences y x) + (similarity xs y)




