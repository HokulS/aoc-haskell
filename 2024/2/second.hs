import System.IO
import System.Environment

main = do
  args <- getArgs
  contents <- readFile (head args)
  let lineOfFiles = lines contents
  let result = map (map readInt . words) lineOfFiles
  print $ parse result



readInt :: String -> Int
readInt = read

parse :: [[Int]] -> Int
parse [[]] = 0
parse [x] 
  | sum (map check (generateLevels 0 x)) >= 0 = 1
  | otherwise = 0
parse (x:xs)
  | sum (map check (generateLevels 0 x)) >= 1 = 1 + parse xs
  | otherwise = 0 + parse xs

-- Checks single lis if it abides by the rules. Returns 1 if it does. 
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

remove :: Int -> [a] -> [a]
remove _ [] = []
remove 0 (x:xs) = xs
remove n (x:xs) = x : remove (n - 1) xs

generateLevels :: Int -> [Int] -> [[Int]]
generateLevels count x
  | count < (length x - 1) = remove count x : generateLevels (count + 1) x
  | otherwise = [remove (length x - 1) x]
