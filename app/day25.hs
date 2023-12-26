module Main where

import           AoC                 (applyInput, blanksP)
import           Data.Bifunctor      (second)
import qualified Data.HashMap.Strict as Map
import           Data.List           (foldl')
import           System.Random       (uniformR, mkStdGen)
import           Text.Parsec         (many1, newline, letter, sepEndBy1, spaces, string)
import           Text.Parsec.String  (Parser)


type Graph = Map.HashMap String (Map.HashMap String Int)


-- TODO : kargers is very slow. The edge list should not be calculated in each step
kargers :: Graph -> Map.HashMap String (Int, Map.HashMap String Int)
kargers g0 = aux (mkStdGen 2023) gStart
  where
    gStart = Map.map (1,) g0

    aux gen g
        | Map.size g == 2 && all (all (==3) . snd) (Map.elems g) = g
        | Map.size g == 2 = aux gen gStart
        | otherwise =
            let (posR, nextGen) = uniformR (0, maxBound) gen
                edges = map (second (Map.toList . snd)) $ Map.toList g
                nedges = sum $ map snd $ concatMap snd edges
                pos = posR `mod` nedges
                edge = getEdge pos edges
            in aux nextGen (removeEdge edge g)

    getEdge _ [] = error "no edge"
    getEdge n ((src, (d, dn) :ds) : rest)
        | dn < n = getEdge (n - dn) ((src, ds) : rest)
        | otherwise = (src, d)
    getEdge n ((_, []): rest) = getEdge n rest

    insertAdd = Map.insertWith (+)

    removeEdge (src, dst) g =
        let dstEdges = Map.delete src $ snd $ g Map.! dst
            dstNodes = fst $ g Map.! dst
            gSt = Map.update (\(n, m) -> Just (n + dstNodes, Map.delete dst m)) src $ Map.delete dst g
            fused =
                foldl'
                    (\gg (d, n) -> Map.update (Just . second (insertAdd src n . Map.delete dst)) d gg)
                    gSt
                    (Map.toList dstEdges)
        in foldl'
                (\gg (d, n) -> Map.update (Just . second (insertAdd d n)) src gg)
                fused
                (Map.toList dstEdges)


solveP1 :: Graph -> Int
solveP1 = product . Map.map fst . kargers


graphP :: Parser Graph
graphP = do
    nodes <- nodeP `sepEndBy1` newline
    pure $ foldl' addNode Map.empty nodes
  where
    nodeP = do
        src <- many1 letter
        dsts <- string ":" >> spaces >> many1 letter `sepEndBy1` blanksP
        pure (src, dsts)

    addNode m (src, dsts) =
        let directM = Map.insertWith (Map.unionWith (+)) src (Map.fromList ((,1) <$> dsts)) m
        in foldl' (\m2 dst -> Map.insertWith (Map.unionWith (+)) dst (Map.singleton src 1) m2) directM dsts


main :: IO ()
main = applyInput graphP solveP1 (const ' ')