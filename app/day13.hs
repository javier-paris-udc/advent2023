module Main where


import AoC (applyInput)
import Text.Parsec.String (Parser)
import Text.Parsec (char, choice, many1, newline, sepEndBy1, spaces)
import Control.Monad (mplus)
import Data.List ( transpose )
import Data.Function ((&))


type Pattern = [[Bool]]


solvePattern :: (Pattern -> Pattern -> Bool) -> Pattern -> Int
solvePattern check pat =
    case fmap (*100) (solve pat) `mplus` solve (transpose pat) of
        Nothing  -> error "solvePattern: no mirroring pattern"
        Just sol -> sol
  where
    solve []     = error "solve: empty pattern"
    solve (p:ps) = findMirror [p] ps

    findMirror _ [] = Nothing
    findMirror side1 side2@(p:ps)
        | check side1 side2 = Just $ length side1
        | otherwise         = findMirror (p:side1) ps


solveP2 :: [Pattern] -> Int
solveP2 = sum . map (solvePattern check)
  where
    check side1 side2 = (==1) $ sum $ zipWith countNEq side1 side2

    countNEq l1 l2 = zipWith (/=) l1 l2
                   & filter id
                   & length


solveP1 :: [Pattern] -> Int
solveP1 = sum . map (solvePattern check)
  where
    check side1 side2 = and $ zipWith (==) side1 side2


patternsP :: Parser [Pattern]
patternsP = patternP `sepEndBy1` spaces
  where
    patternP = lineP `sepEndBy1` newline

    lineP = many1 $ choice [char '#' >> pure True
                           ,char '.' >> pure False]


main :: IO ()
main = applyInput patternsP solveP1 solveP2