import System.Environment
import Data.List

countChar :: String -> Char -> Int
countChar s c = length $ findIndices (==c) s

hasEnoughVowels :: String -> Bool
hasEnoughVowels s = (countChar s 'a' + countChar s 'e' + countChar s 'i' + countChar s 'o' + countChar s 'u') >= 3

containsDoubles :: String -> Bool
containsDoubles s = length (filter (\x -> (length x) > 1) (group s)) > 0

containsBadStrings :: String -> Bool
containsBadStrings s = isInfixOf "ab" s || isInfixOf "cd" s || isInfixOf "pq" s || isInfixOf "xy" s

checkStrings :: [String] -> Int -> Int
checkStrings [] n = n
checkStrings (x:xs) n = if (hasEnoughVowels x) && (containsDoubles x) && (not (containsBadStrings x))
                            then checkStrings xs (n + 1)
                            else checkStrings xs n

main :: IO ()
main = do
    (path:_) <- getArgs
    x <- readFile path
    putStrLn $ show $ checkStrings (lines x) 0
