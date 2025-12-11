module Main where
import Text.Regex.TDFA
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile (head args)
  let textMatches = getAllTextMatches $ contents =~ regex :: [String]
  let filtered = keepEnabled textMatches
  let tuples = map convertTuple filtered
  let values = map (map readInt) tuples
  let result = sum $ map product values
  print result

regex :: String
regex = "mul\\([0-9]{1,3},[0-9]{1,3}\\)|do\\(\\)|don't\\(\\)"

number :: String
number = "[0-9]{1,3}"

readInt :: String -> Int
readInt = read

convertTuple :: String -> [String]
convertTuple x = getAllTextMatches $ x =~ number  :: [String]

keepEnabled :: [String] -> [String]
keepEnabled [] = []
keepEnabled (x:xs)
  | x == "don't()" = removeDisabled xs
  | otherwise = x : keepEnabled xs

removeDisabled :: [String] -> [String]
removeDisabled [] = []
removeDisabled (x:xs)
  | x == "do()" = keepEnabled xs
  | otherwise = removeDisabled xs



