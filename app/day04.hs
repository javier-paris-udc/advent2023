module Main where

import AoC                (applyInput, intP)
import Data.Bits          (shift)
import Data.List          (sort)
import Text.Parsec        (many1, sepEndBy1, spaces, string)
import Text.Parsec.String (Parser)


type Card = ([Int], [Int])

cardMatches :: Card -> Int
cardMatches (win, nums) = matches (sort win) (sort nums)
  where
    matches [] _ = 0
    matches _ [] = 0
    matches (w1:ws) (n1:ns)
        | w1 == n1  = 1 + matches ws ns
        | w1 <  n1  = matches ws (n1:ns)
        | otherwise = matches (w1:ws) ns


solveP2 :: [Card] -> Int
solveP2 = sumCards . map (1,)
  where
    sumCards [] = 0
    sumCards ((n, c):cs) =
        let sc = cardMatches c in
        n + sumCards (addNTo n sc cs)

    addNTo _ 0 l = l
    addNTo _ _ [] = []
    addNTo n x ((num,c):cs) = (num+n, c):addNTo n (x-1) cs


solveP1 :: [Card] -> Int
solveP1 = sum . map (score . cardMatches)
  where
    score 0 = 0
    score n = 1 `shift` (n-1)


cardsP :: Parser [Card]
cardsP = many1 cardP
  where
    cardP = do
        _ <- string "Card" >> spaces
        _ <- intP
        _ <- string ":" >> spaces
        winning <- intP `sepEndBy1` spaces
        _ <- string "| " >> spaces
        numbers <- intP `sepEndBy1` spaces
        pure (winning, numbers)


main :: IO ()
main = applyInput cardsP solveP1 solveP2