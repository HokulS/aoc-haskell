import Data.List

type State = (Int, Int)

type Range = (Int, Int)

splitCommas :: String -> [String]
splitCommas s = case dropWhile (`elem` ",\n") s of
  "" -> []
  s' -> w : splitCommas s''
    where
      (w, s'') = break (`elem` ",\n") s'

splitHyphen :: String -> Range
splitHyphen s = (read x, read ys)
  where
    (x, y : ys) = break (== '-') s

numberOfDigits :: Int -> Int
numberOfDigits = length . show

-- Part 2
isSubString :: String -> Int -> Bool
isSubString x increment = (mod (length x) increment == 0) && all (uncurry (==)) (zip (cycle sub) remain)
  where
    (sub, remain) = splitAt increment x

checkAll :: String -> Bool
checkAll x = any (isSubString x) [1 .. (length x `div` 2)]

go :: State -> Range -> State
go (p1, p2) (x, y) = (p1 + p1', p2 + p2')
  where
    p1' = sum . filter checkInvalid1 $ [x .. y]
      where
        checkInvalid1 z = z `mod` digits == z `div` digits
          where
            digits = 10 ^ (numberOfDigits z `div` 2)
    p2' = sum . filter (checkAll . show) $ [x .. y]

main = do
  (p1, p2) <- foldl' go (0, 0) . map splitHyphen . splitCommas <$> getContents
  print p1
  print p2
