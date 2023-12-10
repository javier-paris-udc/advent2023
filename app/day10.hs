module Main where

import           AoC                (applyInput)
import           Data.Array         (Array, (!))
import qualified Data.Array         as Array
import           Data.Bifunctor     (first, second)
import           Data.HashSet       (HashSet, insert)
import qualified Data.HashSet       as Set
import           Data.List          (find, nub)
import           Data.Maybe         (fromMaybe)
import           Text.Parsec        (char, choice, many1, sepEndBy1, spaces)
import           Text.Parsec.String (Parser)


data Tile = Empty | V | H | NW | NE | SW | SE | Start deriving (Show, Eq)

type Coord = (Int, Int)
type Field = Array Coord Tile


n,s,e,w :: Coord -> Coord
n = first (subtract 1)
s = first (+1)
w = second (subtract 1)
e = second (+1)


neighFuns :: Tile -> [Coord -> Coord]
neighFuns t = case t of
    Empty -> []
    V     -> [n, s]
    H     -> [w, e]
    NW    -> [n, w]
    NE    -> [n, e]
    SW    -> [s, w]
    SE    -> [s, e]
    Start -> [n, s, e, w]

neighs :: Field -> Coord -> [Coord]
neighs field c =
    filter (Array.inRange (Array.bounds field)) $
        neighFuns (field ! c) <*> [c]


insertAll :: [Coord] -> HashSet Coord -> HashSet Coord
insertAll l set = foldr Set.insert set l


findInside :: Field -> [Coord] -> HashSet Coord
findInside field path =
    let inBorder = aux2 path 0 0 Set.empty Set.empty
    in flood inBorder (Set.toList inBorder)
  where
    pathSet = Set.fromList path

    aux2 :: [Coord] -> Int -> Int -> HashSet Coord -> HashSet Coord -> HashSet Coord
    aux2 (c1:c2:cs) left right leftS rightS = case field ! c2 of
        V  -> if s c1 == c2
            then aux2 (c2:cs) left right (insert (e c2) leftS) (insert (w c2) rightS)
            else aux2 (c2:cs) left right (insert (w c2) leftS) (insert (e c2) rightS)
        H  -> if w c1 == c2
            then aux2 (c2:cs) left right (insert (s c2) leftS) (insert (n c2) rightS)
            else aux2 (c2:cs) left right (insert (n c2) leftS) (insert (s c2) rightS)
        NW -> if s c1 == c2
            then aux2 (c2:cs) left (right + 1) (insertAll [s c2, e c2] leftS) rightS
            else aux2 (c2:cs) (left + 1) right leftS (insertAll [s c2, e c2] rightS)
        NE -> if s c1 == c2
            then aux2 (c2:cs) (left + 1) right leftS (insertAll [s c2, w c2] rightS)
            else aux2 (c2:cs) left (right + 1) (insertAll [s c2, w c2] leftS) rightS
        SW -> if n c1 == c2
            then aux2 (c2:cs) (left + 1) right leftS (insertAll [n c2, e c2] rightS)
            else aux2 (c2:cs) left (right + 1) (insertAll [n c2, e c2] leftS) rightS
        SE -> if n c1 == c2
            then aux2 (c2:cs) left (right + 1) (insertAll [n c2, w c2] leftS) rightS
            else aux2 (c2:cs) (left + 1) right leftS (insertAll [n c2, w c2] rightS)
        Empty -> error "findInside: empty"
        Start -> error "findInside: start"
    aux2 _ left right leftS rightS
        | left > right = Set.filter (`notElem` pathSet) leftS
        | otherwise    = Set.filter (`notElem` pathSet) rightS

    flood :: HashSet Coord -> [Coord] -> HashSet Coord
    flood ins [] = ins
    flood ins l  =
        let newCs = nub $ filter (\c -> c `notElem` pathSet && c `notElem` ins) $ [n, s, e, w] <*> l
        in flood (foldr Set.insert ins newCs) newCs


findPath :: Field -> Coord -> [Coord]
findPath field st = foldr (\c rest -> fromMaybe rest (explore st c [st])) (error "No cycle") possible_paths
  where
    possible_paths = filter (\c -> st `elem` neighs field c) $ neighs field st

    explore prev c path
        | c == st   = Just $ reverse path
        | otherwise =
            case find (/= prev) (neighs field c) of
                Nothing   -> Nothing
                Just newC -> explore c newC (c:path)


findStart :: Field -> Coord
findStart field =
    case find (\c -> field ! c == Start) (Array.indices field) of
        Nothing -> error "No start"
        Just c -> c


solveP2 :: Field -> Int
solveP2 field = Set.size inCs
  where
    path = findPath field (findStart field)
    inCs = findInside field path


solveP1 :: Field -> Int
solveP1 field = length (findPath field (findStart field)) `div` 2


pipesP :: Parser Field
pipesP = do
    rows <- many1 tileP `sepEndBy1` spaces
    let nrows = length rows
        ncols = case rows of
            [] -> 0
            r1:_ -> length r1
    pure $ Array.listArray ((0, 0), (nrows-1 , ncols-1)) (concat rows)
  where
    tileP = choice [char '.' >> pure Empty
                   ,char '|' >> pure V
                   ,char '-' >> pure H
                   ,char 'F' >> pure SE
                   ,char '7' >> pure SW
                   ,char 'J' >> pure NW
                   ,char 'L' >> pure NE
                   ,char 'S' >> pure Start]


main :: IO ()
main = applyInput pipesP solveP1 solveP2
