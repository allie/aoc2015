import System.Environment
import Data.List

literals_ :: String -> String -> Int
literals_ [] s = length s
literals_ ('\"':rest) s = literals_ rest s
literals_ ('\\':'\"':rest) s = literals_ rest $ s ++ "_"
literals_ ('\\':'x':a:b:rest) s = literals_ rest $ s ++ "_"
literals_ ('\\':'\\':rest) s = literals_ rest $ s ++ "_"
literals_ (x:rest) s = literals_ rest $ s ++ [x]

literals :: String -> Int
literals s = literals_ s ""

calculate_ :: [String] -> Int -> Int
calculate_ [] n = n
calculate_ (x:rest) n =
    let l = (length x) - (literals x)
        in calculate_ rest $ n + l

calculate :: [String] -> Int
calculate s = calculate_ s 0

main = do
    (path:_) <- getArgs
    x <- readFile path
    putStrLn $ show $ calculate $ lines x
