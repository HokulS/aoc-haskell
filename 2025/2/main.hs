type State = (Int, Int)

type Range = (Int, Int)

main = do
  (p1, p2) <- foldl go (0, 0) . map splitHyphen . splitCommas <$> getContents
  print p1
  print p2

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

go :: State -> Range -> State
go (p1, p2) (x, y) = (p1 + p1', p2 + p2')
  where
    p1' = sum . filter (\z -> z `mod` digits z == z `div` digits z) $ [x .. y]
      where
        digits z = 10 ^ (numberOfDigits z `div` 2)
    p2' = 1
