import System.Environment

move :: (Int, Int) -> Char -> (Int, Int)
move (x, y) d
    | d == '<' = (x - 1, y)
    | d == '>' = (x + 1, y)
    | d == '^' = (x, y + 1)
    | d == 'v' = (x, y - 1)
    | otherwise = (x, y)

-- Input -> Visited coords -> Current coords -> Output
deliveries :: String -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
deliveries [] v c = v
deliveries (x:xs) v c = if elem c v
                            then deliveries xs v (move c x)
                            else deliveries xs (v ++ [c]) (move c x)

main :: IO ()
main = do
    (path:_) <- getArgs
    x <- readFile path
    putStrLn $ show $ length $ deliveries x [] (0, 0)
