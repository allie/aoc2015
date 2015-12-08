import System.Environment
import Data.List
import Data.Maybe

data Light = Light {
    x      :: Int,
    y      :: Int,
    status :: Bool
} deriving Show

type Grid = [Light]

data Op =
      On
    | Off
    | Toggle deriving Show

data Command = Command {
    x1 :: Int,
    y1 :: Int,
    x2 :: Int,
    y2 :: Int,
    op :: Op
} deriving Show

editLights :: Grid -> Command -> Grid
editLights g c = [
    Light {
        x = (x l),
        y = (y l),
        status = if (x l) >= (x1 c) && (x l) <= (x2 c) && (y l) >= (y1 c) && (y l) <= (y2 c)
                     then case (op c) of {
                              On -> True;
                              Off -> False;
                              Toggle -> (not (status l))
                          }
                     else status l
    } | l <- g ]

countEnabledLights :: Grid -> Int
countEnabledLights g = length $ filter status g

-- Parse coordinates given in the form of x,y
parseNumbers :: String -> (Int, Int)
parseNumbers s = let i = fromMaybe 0 $ elemIndex ',' s
                     s1 = splitAt i s
                     l = fst s1
                     r = drop 1 $ snd s1
                     in (read l :: Int, read r :: Int)

parseCommands' :: [String] -> [Command] -> [Command]
parseCommands' [] c = c
parseCommands' (x:xs) c = let w = words x -- Split into words
                              w2 = if (head w) == "turn" then tail w else w -- "Strip 'turn'"
                              o = case (head w2) of -- Operation
                                      "on"     -> On
                                      "off"    -> Off
                                      "toggle" -> Toggle
                              s = parseNumbers (w2 !! 1)
                              e = parseNumbers (w2 !! 3)
                              in parseCommands' xs (c ++ [Command {
                                                              x1 = (fst s),
                                                              y1 = (snd s),
                                                              x2 = (fst e),
                                                              y2 = (snd e),
                                                              op = o
                                                          }])

parseCommands :: [String] -> [Command]
parseCommands s = parseCommands' s []

executeCommands :: [Command] -> Grid -> Grid
executeCommands [] g = g
executeCommands (x:xs) g = executeCommands xs $ editLights g x

initLights :: Int -> Grid
initLights s = [ Light {x = (mod g s), y = (quot g s), status = False} | g <- [0..(s * s)]]

main = do
    (path:_) <- getArgs
    x <- readFile path
    let l = lines x
        g = initLights 1000
        c = parseCommands l
        e = executeCommands c g
    putStrLn $ show $ countEnabledLights e
