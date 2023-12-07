module Main where

import AoC                (applyInput, intP)
import Text.Parsec.String (Parser)
import Data.List          (group, sort, sortBy, sortOn)
import Text.Parsec        (char, choice, many1, sepEndBy1, spaces)
import Control.Category   ((>>>))
import Data.Bifunctor     (first)


data Card = Two | Three | Four | Five | Six | Seven | Eigth | Nine | T | J | Q | K | A
    deriving (Show, Eq, Ord)
newtype CardP2 = CardP2 Card deriving (Show, Eq)


instance Ord CardP2 where
    compare :: CardP2 -> CardP2 -> Ordering
    (CardP2 J)  `compare` (CardP2 J)  = EQ
    (CardP2 J)  `compare` _           = LT
    _           `compare` (CardP2 J)  = GT
    (CardP2 c1) `compare` (CardP2 c2) = c1 `compare` c2


newtype Hand = Hand [Card] deriving (Show, Eq)
newtype HandP2 = HandP2 [CardP2] deriving (Show, Eq)


cardGroups :: (Ord a) => [a] -> [Int]
cardGroups = sort >>> group >>> map length >>> sortBy (flip compare)


instance Ord Hand where
    compare :: Hand -> Hand -> Ordering
    Hand h1 `compare` Hand h2 =
        case cardGroups h1 `compare` cardGroups h2 of
            EQ  -> h1 `compare` h2
            ord -> ord


cardGroupsP2 :: [CardP2] -> [Int]
cardGroupsP2 cards =
    let noJ = filter (/= CardP2 J) cards
        js  = 5 - length noJ
    in addToFirst js $ cardGroups noJ


instance Ord HandP2 where
    compare :: HandP2 -> HandP2 -> Ordering
    HandP2 h1 `compare` HandP2 h2 =
        case cardGroupsP2 h1 `compare` cardGroupsP2 h2 of
            EQ  -> h1 `compare` h2
            ord -> ord


addToFirst :: Int -> [Int] -> [Int]
addToFirst _ []     = [5]
addToFirst n (x:xs) = (x + n):xs


solve :: (Ord a) => [(a, Int)] -> Int
solve = sortOn fst
    >>> map snd
    >>> zip [1..]
    >>> map (uncurry (*))
    >>> sum


solveP2 :: [(Hand, Int)] -> Int
solveP2 = map (first handToHandP2) >>> solve
  where
    handToHandP2 (Hand h) = HandP2 (map CardP2 h)


solveP1 :: [(Hand, Int)] -> Int
solveP1 = solve


handsP :: Parser [(Hand, Int)]
handsP = handP `sepEndBy1` spaces
  where
    handP = do
        hand <- many1 cardP <* spaces
        bid  <- intP
        pure (Hand hand, bid)

    cardP = choice [char '2' >> pure Two
                   ,char '3' >> pure Three
                   ,char '4' >> pure Four
                   ,char '5' >> pure Five
                   ,char '6' >> pure Six
                   ,char '7' >> pure Seven
                   ,char '8' >> pure Eigth
                   ,char '9' >> pure Nine
                   ,char 'T' >> pure T
                   ,char 'J' >> pure J
                   ,char 'Q' >> pure Q
                   ,char 'K' >> pure K
                   ,char 'A' >> pure A
                   ]


main :: IO ()
main = applyInput handsP solveP1 solveP2