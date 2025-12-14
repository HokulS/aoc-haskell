import Data.Char
import Data.List

type ValueIndex = (Int, Int, Int)

findMaxLoc :: ValueIndex -> Int -> ValueIndex
findMaxLoc (value, index, current_index) x
  | value >= x = (value, index, current_index + 1)
  | otherwise = (x, current_index + 1, current_index + 1)

maxVolt :: Int -> [Int] -> Int
maxVolt 0 _ = 0
maxVolt digits xs = ((10 ^ digitsTakeOne) * maxVal) + maxVolt digitsTakeOne (drop location xs)
  where
    digitsTakeOne = digits - 1
    (maxVal, location, _) = foldl' findMaxLoc (0, 0, 0) . take (length xs - digitsTakeOne) $ xs

main = do
  input <- lines <$> getContents
  print . sum . map (maxVolt 2 . map digitToInt) $ input
  print . sum . map (maxVolt 12 . map digitToInt) $ input
