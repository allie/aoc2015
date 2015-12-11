{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

countRepeating :: Char -> BS.ByteString -> Int -> Int
countRepeating _ "" n = n
countRepeating c s n =
    if c == (BS.head s)
        then countRepeating c (BS.drop 1 s) (n + 1)
        else n

buildString :: BS.ByteString -> BS.ByteString -> BS.ByteString
buildString "" b = b
buildString s b =
    let h = BS.head s
        c = countRepeating h s 0
        x = BS.pack $ show c
        in buildString (BS.drop c s) (BS.append (BS.append b x) (BS.singleton h))

calc :: BS.ByteString -> Int -> BS.ByteString
calc s 0 = s
calc s n = calc (buildString s "") (n - 1)

main :: IO ()
main = do
    (path:_) <- getArgs
    x <- readFile path
    let s = BS.pack $ takeWhile (/='\n') x
    putStrLn $ show $ length $ BS.unpack $ calc s 50
