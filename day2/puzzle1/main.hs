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
wrap (l, w, h) = (2*l*w) + (2*w*h) + (2*h*l) + (minimum [l*w, w*h, h*l])

sumPaper :: [(Int, Int, Int)] -> Int -> Int
sumPaper [] n = n
sumPaper (x:xs) n = sumPaper xs (n + (wrap x))

main :: IO ()
main = do
    (path:_) <- getArgs
    x <- readFile path
    let l = listDims (lines x) []
    putStrLn (show (sumPaper l 0))
