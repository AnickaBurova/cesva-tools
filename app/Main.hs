module Main where

import System.Environment
import Lib

main :: IO ()
main = getArgs >>= parse


parse [fp] = do
    putStrLn "\012t\t\tLC1\"\tLA1\"\tLCpeak\tLCt\tLAt\tLCF\tLCFmin\tLCFmax\tLAF\tLAFmin\tLAFmax\tLAS\tLASmin\tLASmax"
    convertNoise fp >>= mapM_ putStrLn
