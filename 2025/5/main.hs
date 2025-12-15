import Data.List

splitHyphen :: String -> (Int, Int)
splitHyphen s = (read x, read ys)
  where
    (x, y : ys) = break (== '-') s

splitInput :: [String] -> ([(Int, Int)], [Int])
splitInput x =
  let (ranges, ingredients) = break (== "") x
   in (map splitHyphen ranges, map read (tail ingredients))

p1 :: [(Int, Int)] -> [Int] -> Int
p1 ranges = length . filter (\x -> any (\(minR, maxR) -> x >= minR && x <= maxR) ranges)

p2 :: [(Int, Int)] -> Int
p2 = fst . foldl' go (0, 0) . sort
  where
    go (acc, curr) (i, j) = (if i > curr then j - i + acc + 1 else (if j < curr then acc else j - curr + acc), max curr j)

main = do
  (ranges, ingredients) <- splitInput . lines <$> getContents
  print . p1 ranges $ ingredients
  print . p2 $ ranges
