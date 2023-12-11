module Main where

import           AoC                (applyInput)
import           Data.HashSet       (HashSet)
import qualified Data.HashSet       as Set
import           Text.Parsec        ((<|>), char, many1, sepEndBy1, spaces)
import           Text.Parsec.String (Parser)


type Coord = (Int, Int)
type GalaxyMap = HashSet Coord


distance :: Coord -> Coord -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)


solve :: Int -> GalaxyMap -> Int
solve gap gm = sum $ distances (Set.toList gm)
  where
    rows = maximum $ Set.map fst gm
    cols = maximum $ Set.map snd gm

    emptyRows = Set.difference (Set.fromList [0 .. rows]) (Set.map fst gm)
    emptyCols = Set.difference (Set.fromList [0 .. cols]) (Set.map snd gm)

    distances []     = []
    distances (g:gs) = map (distanceBetween g) gs ++ distances gs

    distanceBetween g1 g2 = distance g1 g2 + expansionBetween g1 g2
    expansionBetween (x1, y1) (x2, y2) =
        (gap - 1) * Set.size (Set.filter (\r -> r >= min x1 x2 && r <= max x1 x2) emptyRows) +
        (gap - 1) * Set.size (Set.filter (\c -> c >= min y1 y2 && c <= max y1 y2) emptyCols)


solveP2 :: GalaxyMap -> Int
solveP2 = solve 1_000_000


solveP1 :: GalaxyMap -> Int
solveP1 = solve 2


galaxiesP :: Parser GalaxyMap
galaxiesP = do
    rows <- many1 (char '.' <|> char '#') `sepEndBy1` spaces
    let numbered = zipWith (\r -> zipWith (\c -> ((r, c),))  [0..]) [0..] rows
    let galaxies = map fst $ filter ((=='#'). snd) $ concat numbered
    pure $ Set.fromList galaxies


main :: IO ()
main = applyInput galaxiesP solveP1 solveP2
