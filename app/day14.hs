{-# LANGUAGE DeriveGeneric #-}
module Main where


import           AoC                 (applyInput)
import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap, (!?))
import qualified Data.HashMap.Strict as Map
import           Data.List           (foldl')
import           GHC.Generics        (Generic)
import           Text.Parsec         (char, choice, many1, sepEndBy1, spaces)
import           Text.Parsec.String  (Parser)


type Coord = (Int, Int)
data Tile = R | C | E deriving (Show, Eq, Generic) -- Round, Cube, Empty
type Platform = HashMap Coord Tile


instance Hashable Tile


doDir :: (Int -> Int -> Coord) -> (Int -> Int) -> Int -> [Int] -> Int -> Platform -> Platform
doDir coordFun inc d1Size d2s base iniMap =
    foldl' moveRow Map.empty [0 .. d1Size]
  where
    moveRow m c1 =
        fst $ foldl' (move c1) (m, base) d2s

    move c1 (m, curBase) c2 =
        case iniMap !? coordFun c1 c2 of
            Nothing -> (m, curBase)
            Just R  -> (Map.insert (coordFun c1 curBase) R m, inc curBase)
            Just C  -> (Map.insert (coordFun c1 c2) C m, inc c2)
            Just E  -> (m, curBase)


north, south, west, east :: Int -> Int -> Platform -> Platform
north rows cols = doDir (flip (,)) (+1)         cols [0 .. rows - 1]           0
south rows cols = doDir (flip (,)) (subtract 1) cols [rows - 1, rows - 2 .. 0] (rows - 1)
west  rows cols = doDir (,)        (+1)         rows [0 .. cols - 1]           0
east  rows cols = doDir (,)        (subtract 1) rows [cols - 1, cols - 2 .. 0] (cols - 1)


rotate :: Int -> Int -> Platform -> Platform
rotate rows cols plat = east rows cols $ south rows cols $ west rows cols $ north rows cols plat


platformCost :: Int -> Int -> Platform -> Int
platformCost rows cols plat = foldl' colCost 0 [0 .. cols - 1]
  where
    colCost c j = foldl' (cellCost j) c [0 .. rows - 1]
    cellCost j c i =
        case plat !? (i, j) of
            Just R  -> c + rows - i
            _       -> c


solveP2 :: Platform -> Int
solveP2 plat =
    let (m, cycleEnd, cycleStart) = aux plat Map.empty 0
        idx = cycleStart + (1_000_000_000 - cycleStart) `mod` (cycleEnd - cycleStart)
    in case Map.keys $ Map.filter (==idx) m of
        [pl] -> platformCost rows cols pl
        _     -> error "solveP2: Bad index"
  where
    keys = Map.keys plat
    rows = 1 + maximum (map fst keys)
    cols = 1 + maximum (map snd keys)

    aux :: Platform -> HashMap Platform Int -> Int -> (HashMap Platform Int, Int, Int)
    aux p m n
        | p `Map.member` m = maybe (error "solveP2: bad map") (m, n,) (m !? p)
        | otherwise = aux (rotate rows cols p) (Map.insert p n m) (n+1)


solveP1 :: Platform -> Int
solveP1 plat = platformCost rows cols $ north rows cols plat
  where
    keys = Map.keys plat
    rows = 1 + maximum (map fst keys)
    cols = 1 + maximum (map snd keys)


rocksP :: Parser Platform
rocksP = do
    rows <- rowP `sepEndBy1` spaces
    let numbered = zipWith (\i r -> zipWith (\j x -> ((i, j), x)) [0..] r) [0..] rows
    pure $ Map.fromList (concat numbered)
  where
    rowP = many1 $ choice
            [ char 'O' >> pure R
            , char '#' >> pure C
            , char '.' >> pure E
            ]


main :: IO ()
main = applyInput rocksP solveP1 solveP2
