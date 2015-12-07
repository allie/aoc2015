import System.Environment

upOrDown :: Char -> Int
upOrDown n
    | n == '(' = 1
    | n == ')' = -1
    | otherwise = 0

calculate :: String -> Int -> Int
calculate [] n = n
calculate (x:xs) n = calculate xs (n + upOrDown x)

main :: IO ()
main = do
    (path:_) <- getArgs
    x <- readFile path
    putStrLn (show (calculate x 0))
