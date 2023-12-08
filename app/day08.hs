module Main where

import           AoC                 (applyInput)
import           Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as Map
import           Text.Parsec         (alphaNum, char, choice, many1, sepEndBy1, spaces)
import           Text.Parsec.String  (Parser)


type Dir = (String, String) -> String


findCycle :: (String -> Bool) -> [Dir] -> HashMap String (String, String) -> String -> (Int, Int)
findCycle isEnd dirs m start =
    case until isCycle doStep (numbered_dirs, start, Map.empty) of
        ([], _, _) -> error "findCycle: empty list"
        ((pos, cyclePos, _):_, node, visited) -> let fstPos = visited ! (cyclePos, node) in (fstPos, pos)
  where
    numbered_dirs :: [(Int, Int, Dir)] = zipWith (\a (b, c) -> (a, b, c)) [0..] (cycle $ zip [0..] dirs)

    isCycle ((_, cyclePos, _):_, node, visited) = (cyclePos, node) `Map.member` visited
    isCycle _ = error "is_cycle: empty list"

    doStep ((pos, cyclePos, d):rest, node, visited) =
        let newNode = d (m ! node)
        in if isEnd node
           then (rest, newNode, Map.insert (cyclePos, node) pos visited)
           else (rest, newNode, visited)
    doStep _ = error "doStep: empty list"


solveP2 :: ([Dir], HashMap String (String, String)) -> Int
solveP2 (dirs, m) = foldl lcm 1 $ map (fst . findCycle ((=='Z') . last) dirs m) starts
  where
    starts = filter ((=='A') . last) $ Map.keys m


solveP1 :: ([Dir], HashMap String (String,String)) -> Int
solveP1 (dirs, m) = fst $ findCycle (=="ZZZ") dirs m "AAA"


mazeP :: Parser ([Dir], HashMap String (String, String))
mazeP = do
    dirs <- many1 (choice [char 'L' >> pure fst, char 'R' >> pure snd])
    spaces
    nodes <- nodeP `sepEndBy1` spaces
    pure (dirs, Map.fromList nodes)
  where
    nodeP = do
        src  <- many1 alphaNum <* spaces <* char '=' <* spaces
        dstL <- char '(' >> many1 alphaNum
        dstR <- char ',' >> spaces >> many1 alphaNum <* char ')'
        pure (src, (dstL, dstR))


main :: IO ()
main = applyInput mazeP solveP1 solveP2