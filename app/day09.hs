module Main where

import AoC                (applyInput, blanksP, intP)
import Text.Parsec        (sepEndBy1, spaces)
import Text.Parsec.String (Parser)


solveP2 :: [[Int]] -> Int
solveP2 = sum . map findInc
  where
    findInc = sum . concatMap (take 1) . incLists
    incLists l
        | all (==0) l = []
        | otherwise   = l : incLists (zipWith subtract (drop 1 l) l)


solveP1 :: [[Int]] -> Int
solveP1 = solveP2 . map reverse


sensorsP :: Parser [[Int]]
sensorsP = (intP `sepEndBy1` blanksP) `sepEndBy1` spaces


main :: IO ()
main = applyInput sensorsP solveP1 solveP2