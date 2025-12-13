main :: IO ()
main = do
  short <- readFile "short.txt"
  print $ partOne short
  (dial, p2) <- return . foldl partTwo (50, 0) . map readRot . lines $ short
  print p2

  long <- readFile "long.txt"
  print $ partOne long
  (dial, p2) <- return . foldl partTwo (50, 0) . map readRot . lines $ long
  print p2

-- Part One
parseDirection :: String -> Int
parseDirection (x : xs) = if x == 'L' then -(read xs) else read xs

partOne :: String -> Int
partOne = length . filter (== 0) . map (`mod` 100) . scanl (+) 50 . map parseDirection . lines

-- Part Two
type Rot = (Int -> Int, Int)

type State = (Int, Int)

readRot :: String -> Rot
readRot (d : s) = (if d == 'R' then id else negate, read s)

partTwo :: State -> Rot -> State
partTwo (dial, acc) (f, n) = (dial', acc + change)
  where
    dial' = (dial + f n) `mod` 100
    change = (f dial `mod` 100 + n) `div` 100
