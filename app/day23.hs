module Main where


import           AoC                (applyInput)
import           Data.Array         (Array, (!), bounds, indices, inRange, listArray)
import           Data.Bifunctor     (first, second)
import           Data.Function      ((&))
import           Data.IntMap.Strict (IntMap, (!?))
import qualified Data.IntMap.Strict as Map
import           Data.List          (find)
import qualified Data.Map           as GMap
import           Data.Maybe         (fromMaybe, mapMaybe)
import qualified Data.Set           as Set
import           Text.Parsec        (many1, oneOf, sepEndBy1, spaces)
import           Text.Parsec.String (Parser)


type Coord = (Int, Int)
type Graph = IntMap (IntMap Int)


north, south, east, west :: Coord -> Coord
north = first (subtract 1)
south = first (+1)
west  = second (subtract 1)
east  = second (+1)


dirs :: Char -> [Coord -> Coord]
dirs '>' = [east]
dirs '<' = [west]
dirs 'v' = [south]
dirs '^' = [north]
dirs _   = [north, south, west, east]


mapToGraph :: (Char -> [Coord -> Coord]) -> Array Coord Char -> (Int, Int, Graph)
mapToGraph dirsFun m = (0, 1, Map.fromList (zip [0..] (map explore nodes)))
  where
    (maxX, maxY) = snd $ bounds m
    st  = maybe (error "no start") (0, ) (find (\j -> m ! (0, j) == '.') [0 .. maxY])
    end = maybe (error "no start") (maxX,) (find (\j -> m ! (maxX, j) == '.') [0 .. maxY])

    nodes = st : end : filter isNode (indices m)
    nodeMap = GMap.fromList $ zip nodes [0..]

    isNode c
         = m ! c == '.'
        && 2 < length (filter (\c2 -> inRange (bounds m) c2 && m ! c2 `elem` "<>v")
                              (($ c) <$> [north, south, west, east]))

    explore c = Map.fromList (concatMap (expand 1 c) (moves c c))

    -- We only have to check the previous one because the only way to loop is through nodes,
    -- and we stop on those
    moves c prev
        = ($ c) <$> dirsFun (m ! c)
        & filter (\c2 -> bounds m `inRange` c2 && c2 /= prev && m ! c2 /= '#')

    expand n prev c
        | c `elem` nodes = [(nodeMap GMap.! c, n)]
        | otherwise = concatMap (expand (n + 1) c) (moves c prev)


maxPath :: Int -> Int -> Graph -> Int
maxPath st end graph = fromMaybe (error "no path") $ travel st Set.empty
  where
    travel c visited
        | c == end = Just 0
        | c `Set.member` visited = Nothing
        | otherwise = case graph !? c of
            Nothing -> error "no node"
            Just links ->
                case mapMaybe (\(dst, cost) -> (cost +) <$> travel dst (Set.insert c visited))
                              (Map.toList links)
                of
                    [] -> Nothing
                    l  -> Just $ maximum l


solveP2 :: Array Coord Char -> Int
solveP2 m = maxPath st end graph
  where
    (st, end, graph) = mapToGraph (const [north, south, east, west]) m


solveP1 :: Array Coord Char -> Int
solveP1 m = maxPath st end graph
  where
    (st, end, graph) = mapToGraph dirs m


mapP :: Parser (Array Coord Char)
mapP = do
    rows <- rowP `sepEndBy1` spaces
    let nrows = length rows
        ncols = case rows of
            [] -> 0
            r:_ -> length r
    pure $ listArray ((0, 0), (nrows - 1, ncols - 1)) $ concat rows
  where
    rowP = many1 $ oneOf "#.><v^"


main :: IO ()
main = applyInput mapP solveP1 solveP2