module Main where

import Lib

main :: IO ()
main = do
    putStrLn "\012t\t\tLC1\"\tLA1\"\tLCpeak\tLCt\tLAt\tLCF\tLCFmin\tLCFmax\tLAF\tLAFmin\tLAFmax\tLAS\tLASmin\tLASmax"
    convertNoise "CAPTURE.TXT" >>= mapM_ putStrLn. take 3
    --convertNoise "CAPTURE.TXT" >>= print.length
