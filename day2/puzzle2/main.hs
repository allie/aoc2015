import System.Environment

toTuple :: [String] -> (Int, Int, Int)
toTuple [] = (0, 0, 0)
toTuple [l, w, h] = (read l :: Int, read w :: Int, read h :: Int)
toTuple _ = (0, 0, 0)

splitDims :: String -> (Int, Int, Int)
splitDims [] = (0, 0, 0)
splitDims s =
    let replace 'x' = ' '
        replace c = c
    in toTuple $ words $ map replace s

listDims :: [String] -> [(Int, Int, Int)] -> [(Int, Int, Int)]
listDims [] l = l
listDims (x:xs) l = l ++ (listDims xs [splitDims x])

wrap :: (Int, Int, Int) -> Int
wrap (l, w, h) = l*w*h + (minimum [(2*l) + (2*w), (2*w) + (2*h), (2*h) + (2*l)])

sumRibbon :: [(Int, Int, Int)] -> Int -> Int
sumRibbon [] n = n
sumRibbon (x:xs) n = sumRibbon xs (n + (wrap x))

main :: IO ()
main = do
    (path:_) <- getArgs
    x <- readFile path
    let l = listDims (lines x) []
    putStrLn (show (sumRibbon l 0))
