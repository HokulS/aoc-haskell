module Main where
import System.Environment
main :: IO ()
main = do
  dayOne 
  dayTwo

dayOne :: IO ()
dayOne = do
  args <- getArgs
  contents <- readFile $ head args
  let row = sum $ map (readXmasRow 0) $ lines contents
  let row_reverse = sum $ map (readXmasRow 0) $ reverseRows $ lines contents

  let collumn = sum $ map (readXmasRow 0) $ transposeRows $ lines contents
  let collumn_reverse = sum $ map (readXmasRow 0) $ reverseRows $ transposeRows $ lines contents

  let diagonal = sum $ map (readXmasRow 0) $ diagonals $ lines contents
  let diagonal_rev = sum $ map (readXmasRow 0) $ reverseRows $  diagonals  $ lines contents

  let rev_diagonal = sum $ map (readXmasRow 0) $ diagonals $ reverseRows $ lines contents
  let rev_diagonal_rev = sum $ map (readXmasRow 0) $ reverseRows  $ diagonals $ reverseRows $ lines contents

  print $ row + row_reverse + collumn + collumn_reverse + diagonal + diagonal_rev + rev_diagonal + rev_diagonal_rev

dayTwo :: IO()
dayTwo = do
  args <- getArgs
  contents <- readFile $ head args
  let array = lines contents

  let h = map splitAt3 (split array)
  print $ sum $  map findMAS h

getNumber :: [[Char]] -> Int
getNumber x = sum $ map (readMasRow 0) x

findMAS :: [[[Char]]] -> Int
findMAS [] = 0
findMAS (x:xs)
    | getNumber (diagonals  x) + getNumber (reverseRows $ diagonals x) + getNumber (diagonals $ reverseRows x) + getNumber (reverseRows $ diagonals $ reverseRows x) == 2 = 1 + findMAS xs
    | otherwise = 0 + findMAS xs


split :: [[a]] -> [[[a]]]
split [_] = [[]]
split x = x : split (tail x)


splitAt3 :: [[a]] -> [[[a]]]
splitAt3 [] = []
splitAt3 [[]] = []
splitAt3 ([_]:_) = []
splitAt3 x = take 3 ( map (take 3) x) : splitAt3 (map tail x)

readMasRow :: Int -> [Char] -> Int
readMasRow n [] = n
readMasRow n [_] = n
readMasRow n [_, _] = n
readMasRow n (m:a:s:xs)
  | [m,a,s] == "MAS" = readMasRow (n + 1) (a:s:xs)
  | otherwise = readMasRow n (a:s:xs)

readXmasRow :: Int -> String -> Int
readXmasRow n [] = n
readXmasRow n [_] = n
readXmasRow n [_,_] = n
readXmasRow n [_,_,_] = n
readXmasRow n (x:m:a:s:xs)
  | [x,m,a,s] == "XMAS" = readXmasRow (n + 1) (m:a:s:xs)
  | otherwise = readXmasRow n (m:a:s:xs)

reverseRows :: [String] -> [String]
reverseRows [] = []
reverseRows x = map reverse x

transposeRows :: [[a]] -> [[a]]
transposeRows ([]:_) = []
transposeRows x = map head x : transposeRows (map tail x)

diagonals :: [[a]] -> [[a]]
diagonals ([]:xss) = xss
diagonals xss = zipWith (++) (map ((:[]) . head) xss ++ repeat []) ([]:diagonals (map tail xss))


