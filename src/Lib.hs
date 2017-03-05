module Lib where

import qualified Data.List as L
import Data.DList
import Data.List.Split


data Tag
    = Time      String
    | LC1       { getFloat :: Float }
    | LA1       { getFloat :: Float }
    | LCpeak    { getFloat :: Float }
    | LCt       { getFloat :: Float }
    | LAt       { getFloat :: Float }
    | LCF       { getFloat :: Float }
    | LCFmin    { getFloat :: Float }
    | LCFmax    { getFloat :: Float }
    | LAF       { getFloat :: Float }
    | LAFmin    { getFloat :: Float }
    | LAFmax    { getFloat :: Float }
    | LAS       { getFloat :: Float }
    | LASmin    { getFloat :: Float }
    | LASmax    { getFloat :: Float }
    deriving (Show)

getValue :: Tag -> String
getValue (Time a) = a
getValue a = show $ getFloat a

convertNoise :: String -> IO[String]

convertNoise fp = do
    content <- readFile fp
    return
        . L.map (L.intercalate "\t" . L.map getValue)
        . chunksOf 15
        . toList
        . snd
        . foldl decode ((Nothing,Nothing), empty)
        . words
        $ content

type Item = ((Maybe (String -> Tag), Maybe (Float -> Tag)), DList Tag)

decode :: Item -> String -> Item
decode (_,res) "t"      = ((Just Time,Nothing)  , res)
decode (_,res) "LC1\""  = ((Nothing,Just LC1   ), res)
decode (_,res) "LA1\""    = ((Nothing,Just LA1   ), res)
decode (_,res) "LCpeak" = ((Nothing,Just LCpeak), res)
decode (_,res) "LCt"    = ((Nothing,Just LCt   ), res)
decode (_,res) "LAt"    = ((Nothing,Just LAt   ), res)
decode (_,res) "LCF"    = ((Nothing,Just LCF   ), res)
decode (_,res) "LCFmin" = ((Nothing,Just LCFmin), res)
decode (_,res) "LCFmax" = ((Nothing,Just LCFmax), res)
decode (_,res) "LAF"    = ((Nothing,Just LAF   ), res)
decode (_,res) "LAFmin" = ((Nothing,Just LAFmin), res)
decode (_,res) "LAFmax" = ((Nothing,Just LAFmax), res)
decode (_,res) "LAS"    = ((Nothing,Just LAS   ), res)
decode (_,res) "LASmin" = ((Nothing,Just LASmin), res)
decode (_,res) "LASmax" = ((Nothing,Just LASmax), res)
decode ((Just f,Nothing) , res) value = ((Nothing,Nothing), snoc res $ f value)
decode ((Nothing, Just f), res) value = ((Nothing,Nothing), snoc res $ f (read value :: Float))

decode _ t = error t

