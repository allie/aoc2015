import System.Environment
import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.Char8 (ByteString, append, pack)

findBestHash :: ByteString -> Int -> (String, Int)
findBestHash b n = let k = show (md5 (append b (pack (show n))))
                       in if (take 6 k) == "000000"
                           then (k, n)
                           else findBestHash b (n + 1)

main :: IO ()
main = do
    (path:_) <- getArgs
    x <- readFile path
    let b = pack $ takeWhile (/='\n') x
    putStrLn $ show $ findBestHash b 1
