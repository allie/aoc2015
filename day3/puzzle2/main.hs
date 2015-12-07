import System.Environment
import Data.List

move :: (Int, Int) -> Char -> (Int, Int)
move (x, y) d
    | d == '<' = (x - 1, y)
    | d == '>' = (x + 1, y)
    | d == '^' = (x, y + 1)
    | d == 'v' = (x, y - 1)
    | otherwise = (x, y)

-- Input -> Visited coords -> Santa coords -> Robot coords -> Output
deliveries :: String -> [(Int, Int)] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
deliveries [] v s r = v
deliveries (x:y:xs) v s r = deliveries xs (v ++ [s] ++ [r]) (move s x) (move r y)

main :: IO ()
main = do
    (path:_) <- getArgs
    x <- readFile path
    putStrLn $ show $ length $ group $ sort $ deliveries x [] (0, 0) (0, 0)
