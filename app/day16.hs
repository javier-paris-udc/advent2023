{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where


import           AoC                        (applyInput)
import           Data.Array                 (Array, (!), bounds, inRange, listArray)
import           Text.Parsec.String         (Parser)
import           Text.Parsec                (char, choice, many1, sepEndBy1, spaces)
import qualified Data.HashSet               as Set
import           Data.HashSet               (HashSet)
import           Control.Monad.State.Strict (State, execState, modify', get)
import           Data.Bifunctor             (first, second)
import           Data.Hashable              (Hashable)
import           GHC.Generics               (Generic)


data Dir = U | D | L | R deriving (Show, Eq, Generic)
data Tile = Empty | MirrorFW | MirrorBW | SplitterV | SplitterH deriving (Show, Eq)
type Coord = (Int, Int)
type Board = Array Coord Tile
type Set = HashSet


instance Hashable Dir

type Energized = Set (Coord, Dir)


-- forward slash reflection
fw :: Dir -> Dir
fw U = R
fw D = L
fw L = D
fw R = U


-- backwards slash reflection
bw :: Dir -> Dir
bw U = L
bw D = R
bw L = U
bw R = D


move :: Dir -> Coord -> Coord
move U = first (subtract 1)
move D = first (+1)
move L = second (subtract 1)
move R = second (+1)


explore :: Board -> Coord -> Dir -> State Energized ()
explore board c dir = do
    set <- get
    if | not (bounds board `inRange` c) -> pure ()
       | (c, dir) `Set.member` set -> pure ()
       | otherwise -> do
            modify' (Set.insert (c, dir))
            mapM_ (\newD -> explore board (move newD c) newD) newDirs
  where
    newDirs = case board ! c of
        Empty     -> [dir]
        MirrorFW  -> [fw dir]
        MirrorBW  -> [bw dir]
        SplitterV -> if dir `elem` [U, D] then [dir] else [U, D]
        SplitterH -> if dir `elem` [L, R] then [dir] else [L, R]


energy :: Board -> Coord -> Dir -> Int
energy board cIni dir = Set.size $ Set.map fst $ execState (explore board cIni dir) Set.empty


solveP2 :: Board -> Int
solveP2 board = maximum $ map (uncurry (energy board)) starts
  where
    ((iniX, iniY), (endX, endY)) = bounds board
    starts = ((\x -> ((x, 0),    R)) <$> [iniX .. endX]) -- from the left
          ++ ((\x -> ((x, endY), L)) <$> [iniX .. endX]) -- from the right
          ++ ((\y -> ((0, y),    D)) <$> [iniY .. endY]) -- from the top
          ++ ((\y -> ((endX, y), U)) <$> [iniY .. endY]) -- from the bottom


solveP1 :: Board -> Int
solveP1 board = energy board (0,0) R


mirrorsP :: Parser Board
mirrorsP = do
    rows <- rowP `sepEndBy1` spaces
    let nrows = length rows
        ncols = case rows of
                    []    -> 0
                    (r:_) -> length r
    pure $ listArray ((0,0), (nrows - 1, ncols - 1)) $ concat rows
  where
    rowP = many1 $ choice
        [ char '.'  >> pure Empty
        , char '/'  >> pure MirrorFW
        , char '\\' >> pure MirrorBW
        , char '|'  >> pure SplitterV
        , char '-'  >> pure SplitterH
        ]


main :: IO ()
main = applyInput mirrorsP solveP1 solveP2