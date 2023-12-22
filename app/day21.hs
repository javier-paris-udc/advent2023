module Main where

import           AoC                 (applyInput)
import           Data.Bifunctor      (first, second)
import           Data.Foldable       (foldl')
import           Data.Function       ((&))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as Set
import           Data.List           (find)
import           Data.Maybe          (fromMaybe)
import           Text.Parsec         ((<|>), char, many1, sepEndBy1, spaces)
import           Text.Parsec.String  (Parser)


type Coord = (Int, Int)


north, south, east, west :: Coord -> Coord
north = first (subtract 1)
south = first (+1)
east  = second (+1)
west  = second (subtract 1)


flood :: Coord -> (Int, Int) -> HashSet Coord -> HashMap Coord Int
flood st (sx, sy) rocks = aux (Set.singleton st) 0 Map.empty
  where
    aux cs n res
        | Set.null cs = res
        | otherwise =
            let newRes = foldl' (\m c -> Map.insert c n m) res cs
                newCs = concatMap (\c -> [north c, south c, east c, west c]) cs
                      & filter (\c -> inField c && not (Map.member c res) && not (Set.member c rocks))
            in aux (Set.fromList newCs) (n + 1) newRes

    inField (x, y) = x >= 0 && y >= 0 && x < sx && y < sy


distance :: Coord -> Coord -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

odds :: HashMap Coord Int -> Int
odds = Map.size . Map.filter odd

evens :: HashMap Coord Int -> Int
evens = Map.size . Map.filter even


addTo0 :: (Ord p, Num a, Num p, Show p) => p -> p -> p -> (a, p)
addTo0 n fOdd fEven =
    let aux times s !x
            | x > fEven = aux (times + 1) (s + x) (x - fOdd - fEven)
            | x == fEven = (times + 1, s + x)
            | otherwise = error "addTo0"
    in aux 0 0 (n - fOdd - fEven)


squaresFrom :: (Int,Int) -> HashSet Coord -> (Int -> Bool) -> Int -> Coord -> Int
squaresFrom size rocks evenness steps ini =
    Map.size (Map.filter (\d -> evenness d && d < steps) (flood ini size rocks))


solveP2 :: (Coord, (Int, Int), HashSet Coord) -> Int
solveP2 (st, size@(rows, cols), rocks) =
        sum
        [ firstRow
        , countSteps even rows (rows `div` 2, 0) -- middle right partial block
        , countSteps even rows (rows `div` 2, cols -1) -- middle left partial block
        , 2 * inside
        , (mapsStraight - 1) * (limitlu + limitru + limitld + limitrd)
        , countSteps even rows (rows - 1, cols `div` 2)
        , countSteps even rows (0, cols `div` 2)
        , countSteps even (rows `div` 2) (rows - 1, cols - 1)
        , countSteps even (rows `div` 2) (rows - 1, 0)
        , countSteps even (rows `div` 2) (0, cols - 1)
        , countSteps even (rows `div` 2) (0, 0)
        ]
  where
    steps = 26501365
    countSteps = squaresFrom size rocks
    mapsStraight  = max 0 $ (steps - rows `div` 2) `div` rows
    firstRow = mapsStraight * fullEven  -- even r0
              + (mapsStraight - 1) * fullOdd
    fullOdd  = Map.size (Map.filter odd (flood st size rocks))
    fullEven = Map.size (Map.filter even (flood st size rocks))
    (insiderows, inside) = addTo0 firstRow fullOdd fullEven
    limitlu = countSteps even (rows `div` 2) (rows - 1, cols - 1)
            + countSteps odd (rows + rows `div` 2) (rows - 1, cols - 1)
    limitru = countSteps even (rows `div` 2) (rows - 1, 0)
            + countSteps odd (rows + rows `div` 2) (rows - 1, 0)
    limitld = countSteps even (rows `div` 2) (0, cols - 1)
            + countSteps odd (rows + rows `div` 2) (0, cols - 1)
    limitrd = countSteps even (rows `div` 2) (0, 0)
            + countSteps odd (rows + rows `div` 2) (0, 0)


solveP1 :: (Coord, (Int, Int), HashSet Coord) -> Int
solveP1 (st, size, rocks) = Map.size $ Map.filter (\l -> even l && l<=64) $ flood st size rocks


fieldP :: Parser (Coord, (Int, Int), HashSet Coord)
fieldP = do
    rows <- many1 (char '#' <|> char '.' <|> char 'S')  `sepEndBy1` spaces
    let nrows = length rows
        ncols = case rows of
            [] -> 0
            (r:_) -> length r
        numbered = concat $ zipWith (\i r -> zipWith (\j c -> ((i,j),c)) [0..] r) [0..] rows
        sCoord = fst $ fromMaybe (error "no start") $ find ((=='S') . snd) numbered
        rocks = map fst $ filter ((=='#') . snd) numbered
    pure (sCoord, (nrows, ncols), Set.fromList rocks)


main :: IO ()
main = applyInput fieldP solveP1 solveP2