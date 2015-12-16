import System.Environment
import Data.List

fid x y = fromIntegral x / fromIntegral y

total :: Int -> Int -> Int -> Int -> Int
total t v f r =
    let d = v * f
        p = f + r
        c = div t p
        m = fid t p - fromIntegral c
        x = fid f p
        e = if m >= x then d else 0
        in c * d + e

parse :: [String] -> [Int] -> Int
parse [] t = maximum t
parse (x:xs) t =
    let w = words x
        s = 2503
        v = read $ w !! 3
        f = read $ w !! 6
        r = read $ w !! 13
        in parse xs $ t ++ [total s v f r]

main :: IO ()
main = do
    (path:_) <- getArgs
    x <- readFile path
    let l = lines x
    putStrLn $ show $ parse l []
