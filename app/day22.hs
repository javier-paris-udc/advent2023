module Main where

import           AoC                 (applyInput, intP, snd3, thd3)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as Set
import           Data.List           (foldl', sortBy, sortOn)
import           Data.Maybe          (mapMaybe)
import           Text.Parsec         (char, sepEndBy1, spaces)
import           Text.Parsec.String  (Parser)


type Coord = (Int, Int, Int)
type BlockMap = HashMap Int [Coord]
type RestMap = HashMap Int (HashSet Int)


compareHeight :: Coord -> Coord -> Ordering
compareHeight (x1, y1, z1) (x2, y2, z2) = compare (z1, x1, y1) (z2, x2, y2)


fall :: BlockMap -> RestMap
fall bm0 = fst $ foldl' doFall (Map.empty, Map.empty) blocks
  where
    blocks = sortOn (minimum . map thd3 . snd) $ Map.toList bm0

    doFall (rm, heights) (n, coords) =
        let zmin  = minimum $ map thd3 coords
            base  = filter (\(_, _, z) -> z == zmin) coords
            rests = map
                (\c@(x, y, _) ->
                    case heights Map.!? (x, y) of
                        Nothing -> (c, 0, Nothing)
                        Just (zBase, block) -> (c, zBase, Just block)
                ) base

            highestRest = maximum $ map snd3 rests
            offset      = zmin - highestRest - 1
            newCoords   = map (\(a, b, c) -> (a, b, c - offset)) coords
            zmax        = maximum $ map thd3 newCoords
            top         = filter (\(_, _, z) -> z == zmax) newCoords
            restBlocks  = Set.fromList $ mapMaybe thd3 $ filter ((==highestRest) . snd3) rests
        in (Map.insert n restBlocks rm, foldl' (\hm (x, y, _) -> Map.insert (x, y) (zmax, n) hm) heights top)


solveP2 :: BlockMap -> Int
solveP2 bm = sum $ Map.mapWithKey (\i _ -> blocks - Map.size (fallBlocks i groundedBm) - 1) groundedBm
  where
    groundedBm = fall bm
    blocks = Map.size bm

    fallBlocks i cm = aux $ Map.delete i cm
      where
        aux m =
            let noDependsCm = Map.filter (\restsOn -> null restsOn || any (`Map.member` m) restsOn) m
            in if Map.size m == Map.size noDependsCm then noDependsCm
               else aux noDependsCm


solveP1 :: BlockMap -> Int
solveP1 bm = Map.size $ Map.filterWithKey (\i _ -> isOnlySupport i) groundedBm
  where
    groundedBm = fall bm
    isOnlySupport i = Map.null $ Map.filter (\s -> i `Set.member` s && Set.size s == 1) groundedBm


sandBlocksP :: Parser BlockMap
sandBlocksP = listToBlockMap <$> (blockP `sepEndBy1` spaces)
  where
    blockP = do
        st <- coordP
        end <- char '~' >> coordP
        pure (st, end)

    coordP = do
        x <- intP
        y <- char ',' >> intP
        z <- char ',' >> intP
        pure (x, y, z)

    expand (x1, y1, z1) (x2, y2, z2)
        | x1 /= x2  = (, y1, z1) <$> [min x1 x2 .. max x1 x2]
        | y1 /= y2  = (x1, , z1) <$> [min y1 y2 .. max y1 y2]
        | otherwise = (x1, y1, ) <$> [min z1 z2 .. max z1 z2]

    listToBlockMap = Map.fromList . zipWith (\n (st, end) -> (n, sortBy compareHeight (expand st end))) [0..]


main :: IO ()
main = applyInput sandBlocksP solveP1 solveP2