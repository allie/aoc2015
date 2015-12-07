import System.Environment
import Data.List

everyOther :: String -> (String, String)
everyOther s = (snd (unzip (filter (\x -> even (fst x)) (zip [0..] s))), snd (unzip (filter (\x -> odd (fst x)) (zip [0..] s))))

containsDoubles :: String -> Bool
containsDoubles s = length (filter (\x -> (length x) > 1) (group s)) > 0

hasDoublesEveryOther :: String -> Bool
hasDoublesEveryOther s = let (x, y) = everyOther s
                             in containsDoubles x || containsDoubles y

adjacentPairs :: String -> [(Char, Char)]
adjacentPairs s = zip s (tail s)

checkDoublePairs :: Int -> String -> [(Char, Char)] -> Bool
checkDoublePairs _ _ [] = False
checkDoublePairs n s (x:xs) = let t = splitAt n s
                                in if elem x (adjacentPairs ((fst t) ++ "  " ++ (drop 2 (snd t))))
                                    then True
                                    else checkDoublePairs (n + 1) s xs

hasDoublePairs :: String -> Bool
hasDoublePairs s = checkDoublePairs 0 s (adjacentPairs s)

checkStrings :: [String] -> Int -> Int
checkStrings [] n = n
checkStrings (x:xs) n = if (hasDoublePairs x) && (hasDoublesEveryOther x)
                            then checkStrings xs (n + 1)
                            else checkStrings xs n

main :: IO ()
main = do
    (path:_) <- getArgs
    x <- readFile path
    putStrLn $ show $ checkStrings (lines x) 0
