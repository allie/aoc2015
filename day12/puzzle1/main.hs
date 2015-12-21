{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

sumNums :: ByteString -> Int -> Int
sumNums "" n = n
sumNums s n =
    let m = BS.readInt s
        in case m of
               Just (i, x) -> sumNums x $ n + i
               Nothing     -> sumNums (BS.tail s) n

main :: IO ()
main = do
    (path:_) <- getArgs
    x <- BS.readFile path
    putStrLn $ show $ sumNums x 0
