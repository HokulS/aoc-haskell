import System.IO 
import System.Environment

main = do
  args <- getArgs
  contents <- readFile (args !! 0)
  let lineOfFiles = lines contents
  let result = (map.map) readInt $ map words lineOfFiles
  
  print $ parse result

readInt :: String -> Int
readInt = read 

parse :: [[Int]] -> Int
parse [[]] = 0 
parse [x] = check x 
parse (x:xs)
  | check x == 1 = 1 + parse xs

check :: [Int] -> Int
check [] = 1
check [_] = 1
check (x:y:xs)
  | increasing (x:y:xs) + decreasing (x:y:xs) == 0 = 0 
  | abs (x - y) < 1 = 0 
  | abs (x - y) > 3 = 0 
  | otherwise = 1 * check (y:xs)

increasing :: [Int] -> Int
increasing [] = 1
increasing [_] = 1
increasing (x:y:xs)
  | x < y = 1 * increasing (y:xs)
  | otherwise = 0 

decreasing :: [Int] -> Int
decreasing [] = 1
decreasing [_] = 1
decreasing (x:y:xs)
  | x > y = 1 * decreasing (y:xs)
  | otherwise = 0 

 





