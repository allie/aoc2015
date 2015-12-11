import System.Environment
import Data.List

encode_ :: String -> String -> Int
encode_ [] s = (length s) + 2
encode_ ('"':rest) s = encode_ rest $ s ++ "\\\""
encode_ ('\\':rest) s = encode_ rest $ s ++ "\\\\"
encode_ (x:rest) s = encode_ rest $ s ++ [x]

encode :: String -> Int
encode s = encode_ s ""

calculate_ :: [String] -> Int -> Int
calculate_ [] n = n
calculate_ (x:rest) n =
    let l = (encode x) - (length x)
        in calculate_ rest $ n + l

calculate :: [String] -> Int
calculate s = calculate_ s 0

main = do
    (path:_) <- getArgs
    x <- readFile path
    putStrLn $ show $ calculate $ lines x
