import Control.Concurrent.STM (check)
import Data.Array

createGrid :: [[Char]] -> Array (Int, Int) Char
createGrid input = array ((1, 1), (rows, cols)) [((i, j), letter) | (i, word) <- zip [1 ..] input, (j, letter) <- zip [1 ..] word]
  where
    rows = length input
    cols = length . head $ input

checkRadius :: Array (Int, Int) Char -> ((Int, Int), Char) -> Bool
checkRadius _ ((_, _), '.') = False
checkRadius array ((x, y), cur) = (length . filter (\index -> array ! index == '@') $ searchIndices array (x, y)) < 4

searchIndices :: Array (Int, Int) Char -> (Int, Int) -> [(Int, Int)]
searchIndices array (x, y) = filter (\(i, j) -> i `elem` [x - 1 .. x + 1] && j `elem` [y - 1 .. y + 1] && (i, j) /= (x, y)) $ indices array

removeableIndices :: Array (Int, Int) Char -> [(Int, Int)]
removeableIndices array = [x | (x, y) <- filter (checkRadius array) (assocs array)]

part2 :: Array (Int, Int) Char -> Int -> Int
part2 x acc
  | null indices_to_remove = acc
  | otherwise = part2 (x // [(i, '.') | i <- indices_to_remove]) (acc + length indices_to_remove)
  where
    indices_to_remove = removeableIndices x

main = do
  grid <- createGrid . lines <$> getContents
  print . length . filter (checkRadius grid) $ assocs grid
  print $ part2 grid 0
