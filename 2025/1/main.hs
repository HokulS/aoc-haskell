main :: IO()
main = do
    short <- readFile "short.txt"
    let short_lines = lines short
    print $ partOne short_lines

    long <- readFile "long.txt"
    let long_lines = lines long
    print $ partOne long_lines

partOne :: [String] -> Int
partOne x = length $ filter (== 0) $ map modulus $ scanl (+) 50 $ map parseDirection x

parseDirection :: String -> Int
parseDirection (x:xs) = if x == 'L' then -1 * read xs else read xs

modulus :: Int -> Int
modulus x = mod x 100
