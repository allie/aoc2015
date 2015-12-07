import System.Environment

upOrDown :: Char -> Int
upOrDown n
    | n == '(' = 1
    | n == ')' = -1
    | otherwise = 0

calculate :: String -> (Int, Int) -> (Int, Int)
calculate _ (-1, p) = (-1, p)
calculate (x:xs) (n, p) = calculate xs (n + upOrDown x, p + 1)

main :: IO ()
main = do
    (path:_) <- getArgs
    x <- readFile path
    let (_, p) = calculate x (0, 0)
    putStrLn (show p)
