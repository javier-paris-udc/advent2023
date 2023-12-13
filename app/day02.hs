{-# LANGUAGE DeriveGeneric #-}
module Main where

import           AoC                 (applyInput, intP)
import           Data.Foldable       (foldl')
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict ((!))
import           GHC.Generics        (Generic)
import           Text.Parsec         (choice, sepBy1, sepEndBy1, spaces, string)
import           Text.Parsec.String  (Parser)


data Color = Red | Blue | Green deriving (Show, Eq, Generic)

instance Hashable Color

type Play = [(Color, Int)]
type Game = [Play]


solveP2 :: [(Int, Game)] -> Int
solveP2 = sum . map (product . minDiceSet . snd)
  where
    minDiceSet = foldl' foldPlay (Map.fromList [(Red, 0), (Green, 0), (Blue, 0)])
    foldPlay   = foldl' (\m (c, n) -> Map.insertWith max c n m)


solveP1 :: [(Int, Game)] -> Int
solveP1 = sum . map fst . filter gameInLimits
  where
    limits = Map.fromList [(Red, 12), (Green, 13), (Blue, 14)]
    gameInLimits = all playInLimits . snd
    playInLimits = all (\(col, n) -> limits ! col >= n)


gamesP :: Parser [(Int, Game)]
gamesP = gameP `sepEndBy1` spaces
  where
    gameP = do
        game_id <- string "Game " >> intP <* string ": "
        game    <- playP `sepBy1` sepP ";"
        pure (game_id, game)

    playP = diceP `sepBy1` sepP ","

    diceP = do
        n <- intP <* spaces
        dice <- choice [string "red"   >> pure Red
                       ,string "blue"  >> pure Blue
                       ,string "green" >> pure Green]
        pure (dice, n)

    sepP sep = string sep >> spaces


main :: IO ()
main = applyInput gamesP solveP1 solveP2