module Main where

import           AoC                 (applyInput, commaSepP, intP)
import           Data.Bifunctor      (bimap)
import           Data.Function       ((&))
import qualified Data.HashMap.Strict as Map
import           Data.List           (intercalate)
import           Text.Parsec         (char, choice, many1, sepBy1, sepEndBy1, spaces)
import           Text.Parsec.String  (Parser)


data Spring = W | B | U deriving (Show, Eq) -- Working, Broken, Unknown

possible :: [Spring] -> [Int] -> Int
possible springs nums = aux lenSp $ Map.fromList [((Nothing, lenNs), (1, springs, nums))]
  where
    lenSp :: Int = length springs
    lenNs :: Int = length nums

    aux lenS m
        | lenS == 0 =
              m
            & Map.filterWithKey noNs
            & Map.elems
            & map (\(a,_,_) -> a)
            & sum
        | otherwise = aux (lenS - 1) (Map.foldlWithKey' mix Map.empty m)

    noNs (Nothing, 0) _ = True
    noNs (Just 0,  0) _ = True
    noNs _            _ = False

    insert = Map.insertWith (\(a,b,c) (d,_,_) -> (a+d, b, c))

    mix m (cur, lenN) (cases, sp, ns) =
        case (cur, sp, ns) of
            (_, [], _)       -> error "mix: empty list"
            (Nothing, s:ss, [])
                | s /= B    -> insert (Nothing, lenN) (cases, ss, []) m
                | otherwise -> m
            (Nothing, W:ss, _) -> insert (Nothing, lenN) (cases, ss, ns) m
            (Nothing, B:ss, n:nms) -> insert (Just (n-1), lenN-1) (cases, ss, nms) m
            (Nothing, U:ss, n:nms) -> m
                                    & insert (Just (n-1), lenN-1) (cases, ss, nms)
                                    & insert (Nothing, lenN) (cases, ss, ns)
            (Just 0, W:ss, _) -> insert (Nothing, lenN) (cases, ss, ns) m
            (Just 0, B:_,  _) -> m
            (Just 0, U:ss, _) -> insert (Nothing, lenN) (cases, ss, ns) m
            (Just _, W:_,  _) -> m
            (Just n, B:ss, _) -> insert (Just (n-1), lenN) (cases, ss, ns) m
            (Just n, U:ss, _) -> insert (Just (n-1), lenN) (cases, ss, ns) m


solveP2 :: [([Spring], [Int])] -> Int
solveP2 = solveP1 . map (bimap repeat5QM repeat5)
  where
    repeat5   = concat . replicate 5
    repeat5QM = intercalate [U] . replicate 5


solveP1 :: [([Spring], [Int])] -> Int
solveP1 = sum . map (uncurry possible)


springRowsP :: Parser [([Spring], [Int])]
springRowsP = springRowP `sepEndBy1` spaces
  where
    springRowP = do
        springs <- many1 springP
        nums <- spaces >> intP `sepBy1` commaSepP
        pure (springs, nums)

    springP = choice [char '.' >> pure W
                     ,char '#' >> pure B
                     ,char '?' >> pure U
                     ]


main :: IO ()
main = applyInput springRowsP solveP1 solveP2