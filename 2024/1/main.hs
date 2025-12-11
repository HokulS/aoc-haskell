import System.IO 
import Control.Monad 
import Data.List

main = do 
  contents <- readFile "puzzle.txt"
  let a = map readInt . words $ contents
  let first = evens a
  let second = odds a 
  let sum = totalDistance first second

  print sum



readInt :: String -> Int
readInt = read

evens (x:xs) = x : odds xs
evens [] = [] 

odds (_:xs) = evens xs
odds [] = []

takeMin :: [Int] -> Int
takeMin x = foldr1 min x

removeMin :: [Int] -> [Int]
removeMin x = delete (takeMin x) x

totalDistance :: [Int] -> [Int] -> Int
totalDistance [] [] = 0
totalDistance x y = absolute(takeMin x - takeMin y) + totalDistance (removeMin x) (removeMin y)

absolute :: Int -> Int
absolute x 
  | x > 0 = x
  | x < 0 = -x
  | otherwise = 0

