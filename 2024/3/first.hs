import System.Environment 
import System.IO 
import Text.Regex.Base

main = do
  args <- getArgs 
  contents <- readFile (head args)
  print contents


mul :: Num a => a -> a -> a 
mul x y = x * y
