import System.Environment
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Debug.Trace

type Path = (String, String)

sumDists :: [Path] -> Map Path Int -> Int -> Maybe Int
sumDists [] m n = Just n
sumDists (p:rest) m n =
    let v = Map.lookup p m
        in case v of
               Nothing -> Nothing
               Just x -> sumDists rest m (n + x)

calcShortest :: [[String]] -> Map Path Int -> [Int] -> Int
calcShortest [] m n = maximum n
calcShortest (x:rest) m n =
    let d = zip x $ tail x
        s = sumDists d m 0
        in case s of
               Nothing -> calcShortest rest m n
               Just y -> calcShortest rest m (n ++ [y])

parsePath :: String -> (String, String, Int)
parsePath x =
    let w = words x
        in (head w, w !! 2, read (w !! 4) :: Int)

buildPerms :: [String] -> [String] -> [[String]]
buildPerms [] l = permutations l
buildPerms (x:rest) l =
    let (s, e, _) = parsePath x
        y = if (not (isInfixOf [s] l)) then [s] else []
        z = if (not (isInfixOf [e] l)) then [e] else []
        in buildPerms rest $ l ++ y ++ z

buildMap :: [String] -> Map Path Int -> Map Path Int
buildMap [] m = m
buildMap (x:rest) m =
    let (s, e, d) = parsePath x
        in buildMap rest $ Map.insert (s, e) d $ Map.insert (e, s) d m

main :: IO ()
main = do
    (path:_) <- getArgs
    x <- readFile path
    let l = lines x
        p = buildPerms l []
        m = buildMap l Map.empty
    putStrLn $ show $ calcShortest p m []
