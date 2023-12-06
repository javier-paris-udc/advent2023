module Main where

import AoC                (applyInput, intP)
import Data.Bifunctor     (bimap)
import Data.List          (singleton)
import Text.Parsec        (many1, newline, oneOf, sepEndBy1, spaces, string)
import Text.Parsec.String (Parser)


solveP2 :: [(Int, Int)] -> Int
solveP2 = solveP1 . singleton . bimap (read. concatMap show) (read. concatMap show) . unzip


solveP1 :: [(Int, Int)] -> Int
solveP1 = product . map solveRace
  where
    solveRace (t, d) = let tD   = fromIntegral t :: Double
                           dD   = fromIntegral d :: Double
                           low  = ceiling $ (tD - sqrt (tD**2 - 4*dD)) / 2
                           high = floor   $ (tD + sqrt (tD**2 - 4*dD)) / 2
                       in high - low + 1


racesP :: Parser [(Int, Int)]
racesP = do
    times <- string "Time:" >> spaces >> intP `sepEndBy1` many1 (oneOf "\t ")
    _ <- newline
    distances <- string "Distance:" >> spaces >> intP `sepEndBy1` many1 (oneOf "\t ")
    pure $ zip times distances


main :: IO ()
main = applyInput racesP solveP1 solveP2