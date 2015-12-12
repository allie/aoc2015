{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word
import Data.Bits
import Data.Maybe
import Control.Monad.State
import Debug.Trace

data Operation = NOP | AND | OR | NOT | LSHIFT | RSHIFT deriving (Eq, Ord, Show)
data Input = Input { op :: Operation, operands :: [ByteString] } deriving (Eq, Ord, Show)
data Wire = Wire { input :: Input, output :: Maybe Word16 } deriving (Eq, Ord, Show)
type WireMap = Map ByteString Wire
data CalcState = CalcState { wm :: WireMap } deriving (Eq, Ord, Show)
type Calc a = State CalcState a

modMap :: ByteString -> Wire -> Calc ()
modMap k v = do
    m <- gets wm
    modify (\x -> x { wm = (Map.insert k v m) })

doOp :: [Word16] -> Operation -> Word16
doOp [] _ = trace "doOp - no operands" $ 0
doOp [a] op
    | op == NOP = a
    | op == NOT = complement a
doOp [a,b] op
    | op == NOP    = trace "doOp - 2 operand NOP" $ 0
    | op == AND    = a .&. b
    | op == OR     = a .|. b
    | op == LSHIFT = a `shiftL` i
    | op == RSHIFT = a `shiftR` i
    where i = fromIntegral b :: Int
doOp x op = trace ("doOp - unexpected combination: " ++ (show x) ++ (show op)) $ 0

val :: ByteString -> Calc Word16
val s = do
    let m = BS.readInt s
    case m of
        Just (r, _) -> return (fromIntegral r :: Word16)
        Nothing -> signal s

empty = Wire { input = Input { op = NOP, operands = []}, output = Nothing }

signal :: ByteString -> Calc Word16
signal s = do
    m <- gets wm
    let w = Map.lookup s m
        i = fromMaybe empty w
        v = output i
    case v of
        Just _  -> return $ fromJust v
        Nothing -> do
            let n = input i
                o = op n
                r = operands n
            case (length r) of
                1 -> do
                    r1 <- val (head r)
                    let d = doOp [r1] o
                    modMap s $ i { output = Just d }
                    return d
                2 -> do
                    r1 <- val (head r)
                    r2 <- val (last r)
                    let d = doOp ([r1,r2]) o
                    modMap s $ i { output = Just d }
                    return d

parseSignal :: [ByteString] -> ([ByteString], Operation)
parseSignal [] = ([], NOP)
parseSignal [x] = ([x], NOP)
parseSignal [x,y] = ([y], NOT)
parseSignal [x,y,z] =
    let o = case y of
                "AND"     -> AND
                "OR"      -> OR
                "LSHIFT"  -> LSHIFT
                "RSHIFT"  -> RSHIFT
                otherwise -> NOP
        in ([x,z], o)

parse :: [ByteString] -> Calc ()
parse [] = return ()
parse (x:xs) = do
    let s = BS.words x
        l = last s
        (a, o) = parseSignal $ take ((length s) - 2) s
        i = Input { op = o, operands = a }
        w = Wire { input = i, output = Nothing }
    _ <- modMap l w
    parse xs

calculate :: [ByteString] -> Calc Word16
calculate s = do
    _ <- parse s
    signal "a"

main :: IO ()
main = do
    (path:_) <- getArgs
    x <- BS.readFile path
    let l = BS.lines x
    putStrLn $ show $ evalState (calculate l) $ CalcState { wm = Map.empty }
