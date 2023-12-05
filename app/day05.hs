module Main where

import           AoC                (applyInput, intP)
import           Data.Foldable      (foldl')
import           Data.Sequence      (Seq, (!?))
import qualified Data.Sequence      as Seq
import           Text.Parsec        (letter, many1, newline, sepBy1, sepEndBy1, spaces, string)
import           Text.Parsec.String (Parser)


data IntervalRes =
    None
  | Exact (Int, Int, Int)
  | Greater (Int, Int, Int)


find :: Int -> Seq (Int, Int, Int) -> IntervalRes
find n q = seek 0 (Seq.length q - 1)
  where
    seek left right
        | left > right =
            None
        | otherwise =
            let mid = (right+left) `div` 2 in case q !? mid of
                Nothing -> error "find: mid position does not exists"
                Just tup@(_, midSrcSt, midLen)
                    | n>= midSrcSt && n < midSrcSt + midLen ->
                        Exact tup
                    | n < midSrcSt ->
                            if left == right
                                then Greater tup
                                else seek left (mid - 1)
                    | otherwise ->
                            if left == right
                                then maybe None Greater $ q !? (right + 1)
                                else seek (mid + 1) right


solveP2 :: ([Int], [Seq (Int, Int, Int)]) -> Int
solveP2 (seedL, maps) = minimum $ map fst $ foldl (\s m -> s >>= do_map m) seeds maps
  where
    buildSeeds [] = []
    buildSeeds (x1:x2:xs) = (x1, x2) : buildSeeds xs
    buildSeeds _ = error "Seed List"

    seeds = buildSeeds seedL

    do_map m (st, len)
        | len == 0  = []
        | otherwise = case find st m of
            None -> [(st, len)]
            Greater (_, src, _) ->
                        let newIntervalLen = min len (src - st)
                        in (st, newIntervalLen) : do_map m (src, len - newIntervalLen)
            Exact (dst, src, intervalLen) ->
                let overlap = intervalLen - (st - src)
                    newIntervalLen = min len overlap
                    nextLen = len - newIntervalLen
                in (dst + st - src, newIntervalLen) : do_map m (st + newIntervalLen, nextLen)


solveP1 :: ([Int], [Seq (Int, Int, Int)]) -> Int
solveP1 (seeds, maps) = minimum $ map (\s -> foldl do_map s maps) seeds
  where
    do_map num m =
        case find num m of
            None -> num
            Greater _ -> num
            Exact (dst, src, _) -> dst + (num - src)


insert :: Int -> Int -> Int -> Seq (Int, Int, Int) -> Seq (Int, Int, Int)
insert dstSt srcSt len interSeq = insertAux 0 lastIdx
  where
    lastIdx = Seq.length interSeq - 1
    insertAux left right
        | left > right = Seq.insertAt left (dstSt, srcSt, len) interSeq
        | otherwise    = let mid = (left + right) `div` 2 in
            case interSeq !? mid of
                Nothing -> undefined
                Just (_, midSrcSt, _) ->
                    if midSrcSt > srcSt
                    then insertAux left (mid-1)
                    else insertAux (mid+1) right


mapsP :: Parser ([Int], [Seq (Int, Int, Int)])
mapsP = do
    seeds <- string "seeds: " >> intP `sepBy1` string " "
    spaces
    maps <- mapP `sepEndBy1` spaces
    pure (seeds, maps)
  where
    mapP = do
        many1 letter >> string "-" >> many1 letter >> string "-" >> many1 letter >> string " map:" >> spaces
        tups <- tup3P `sepEndBy1` newline
        return (foldl' (\q (a, b, c) -> insert a b c q) Seq.empty tups)

    tup3P = do
        n1 <- intP
        n2 <- spaces >> intP
        n3 <- spaces >> intP
        pure (n1, n2, n3)


main :: IO ()
main = applyInput mapsP solveP1 solveP2