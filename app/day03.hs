module Main where


import AoC                (applyInput)
import Control.Category   ((>>>))
import Data.Array         (Array, (!), bounds,  Ix (inRange), listArray)
import Data.Bifunctor     (second)
import Data.Char          (digitToInt, isDigit)
import Data.Function      (on)
import Data.List          (find, groupBy, sortOn)
import Data.Maybe         (catMaybes)
import Text.Parsec        (many1, newline, noneOf, sepEndBy1)
import Text.Parsec.String (Parser)


type Coord = (Int, Int)
type Engine = Array Coord Char


findNumbers :: Engine -> [(Int, Maybe (Coord, Char))]
findNumbers arr = concatMap (`row` 0) [ix .. ex]
  where
    ((ix, _iy), (ex, ey)) = bounds arr

    row r y
        | y > ey = []
        | isDigit (arr ! (r, y)) =
            let (n, end) = getNum r y 0
                surr     = neighCoords r y end
                opers    = find ((/= '.') . (arr !)) surr
            in (n, fmap ((,) <*> (arr !)) opers):row r (end + 1)
        | otherwise = row r (y+1)

    getNum x y n
        | y > ey = (n, ey)
        | isDigit (arr ! (x, y)) = getNum x (y+1) (n*10 + digitToInt (arr ! (x, y)))
        | otherwise = (n, y-1)

    neighCoords x fromy toy =
        let allCoords = ((x-1,) <$> [fromy - 1 .. toy + 1])
                      ++[(x, fromy-1) , (x, toy+1)]
                      ++((x+1,) <$> [fromy - 1 .. toy + 1])
        in filter (inRange (bounds arr)) allCoords


solveP2 :: Engine -> Int
solveP2 = findNumbers
      >>> map sequence
      >>> catMaybes
      >>> filter ((=='*') . snd . snd)
      >>> map (second fst)
      >>> sortOn snd
      >>> groupBy ((==) `on` snd)
      >>> filter ((>1) . length)
      >>> map (product . map fst)
      >>> sum


solveP1 :: Engine -> Int
solveP1 = findNumbers
      >>> map sequence
      >>> catMaybes
      >>> map fst
      >>> sum


engineP :: Parser Engine
engineP = do
    engine <- many1 (noneOf "\n\t ") `sepEndBy1` newline
    let rows = length engine
        cols = length (head engine)
    pure $ listArray ((0, 0), (rows-1, cols-1)) (concat engine)


main :: IO ()
main = applyInput engineP solveP1 solveP2