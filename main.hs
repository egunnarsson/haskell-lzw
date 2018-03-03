
import System.Environment
import qualified Data.ByteString as B
import Codec.Compression.LZW

main = getArgs >>= (run decompress)

run _ ("-c" : args) = run compress args
run f ("-i" : input  : "-o" : output : []) = (B.readFile input) >>= ((B.writeFile output) . f)
run f ("-i" : input  : []) = (B.readFile input) >>= (B.putStr . f)
run f ("-o" : output : []) = B.getContents >>= (B.writeFile output . f)
run f [] = B.interact f
run _ _ = usage

usage = putStrLn "Usage: lzw [-c] [-i file] [-o file]"
