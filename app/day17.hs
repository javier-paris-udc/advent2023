{-# LANGUAGE DeriveGeneric #-}
module Main where


import           AoC                (applyInput)
import           Data.Array.Unboxed (Array, (!), bounds, inRange, listArray)
import           Data.Bifunctor     (first, second)
import           Data.Char          (digitToInt)
import           Data.Function      ((&))
import           Data.Hashable      (Hashable)
import qualified Data.HashPSQ       as Q
import qualified Data.HashSet       as Set
import           Data.List          (foldl')
import           Data.Maybe         (fromMaybe)
import           GHC.Generics       (Generic)
import           Text.Parsec        (digit, many1, sepEndBy, spaces)
import           Text.Parsec.String (Parser)


type Coord = (Int, Int)
type City = Array Coord Int
data Dir = U | D | L | R deriving (Show, Eq, Ord, Generic)
type St = (Coord, Dir, Int)

instance Hashable Dir


op :: Dir -> Dir
op U = D
op D = U
op L = R
op R = L


move :: Dir -> Coord -> Coord
move U = first (subtract 1)
move D = first (+1)
move L = second (subtract 1)
move R = second (+1)


dijkstra :: (St -> St -> Bool) -> (St -> Bool) -> (Int -> [Int]) -> City -> Int
dijkstra legalMove isGoal eqCounts city = fromMaybe (error "No path") $ aux qIni Set.empty
  where
    qIni = Q.fromList [(((0,0), R, 0), 0, ()), (((0,0), D, 0), 0, ())]

    aux :: Q.HashPSQ (Coord, Dir, Int) Int () -> Set.HashSet (Coord, Dir, Int) -> Maybe Int
    aux q visited =
        case Q.findMin q of
            Nothing -> Nothing
            Just (st@(c, dir, dirN), cost, ())
                | st `Set.member` visited -> aux (Q.deleteMin q) visited
                | isGoal st -> Just cost
                | otherwise ->
                    let new = newMoves c dir dirN cost visited
                    in aux (insertAll new (Q.deleteMin q))
                           (foldl' (flip Set.insert) visited ((c, dir,) <$> eqCounts dirN))

    newMoves c dir dirN cost visited =
        map (\d -> (move d c, d, if d == dir then dirN + 1 else 1)) [U, D, L, R]
      & filter (\st@(newC, newD, _) ->
               newD /= op dir
            && legalMove (c, dir, dirN) st
            && bounds city `inRange` newC
            && not (Set.member st visited))
      & map (\newSt@(newC, _, _) -> (newSt, cost + city ! newC))

    insertAll new q0 =
        foldl' (\q (st, cost) ->
            case Q.lookup st q of
                Nothing -> Q.insert st cost () q
                Just (oldC, ()) -> if oldC <= cost then q else Q.insert st cost () q
            ) q0 new


solveP2 :: City -> Int
solveP2 city = dijkstra legal isSol eqInts city
  where
    dst = snd $ bounds city
    legal (_, oldD, oldCount) (_, newD, newCount) = (oldD == newD || oldCount >=4) && newCount <= 10
    isSol (c, _, count) = c == dst && count >= 4
    eqInts n
        | n >=4     = [n..10]
        | otherwise = [n]


solveP1 :: City -> Int
solveP1 city = dijkstra legal isSol (\n -> [n .. 3]) city
  where
    dst = snd $ bounds city
    legal _ (_, _, count) = count <= 3
    isSol (c, _, _) = c == dst


cityP :: Parser City
cityP = do
    rows <- (map digitToInt <$> many1 digit) `sepEndBy` spaces
    let nrows = length rows
        ncols = case rows of
            []  -> 0
            r:_ -> length r
    pure $ listArray ((0, 0), (nrows - 1, ncols - 1)) (concat rows)


main :: IO ()
main = applyInput cityP solveP1 solveP2
