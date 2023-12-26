module Main where


import AoC                (applyInput, commaSepP, fst3, intP, snd3, thd3)
import Control.Category   ((>>>))
import Data.Bifunctor     (bimap, first)
import Data.Function      ((&), on)
import Data.List          (groupBy, intersect, nub, sort, sortBy, subsequences)
import Data.Ratio         ((%), numerator)
import Text.Parsec        (char, sepBy1, sepEndBy1, spaces, unexpected)
import Text.Parsec.String (Parser)


type Hail = ((Rational, Rational, Rational), (Rational, Rational, Rational))


gauss :: [[Rational]] -> [Rational]
gauss mtx = aux 0 mtx
  where
    vars = length (head mtx) - 1

    aux :: Int -> [[Rational]] -> [Rational]
    aux v m
        | v == vars = map last m
        | otherwise =
            let eq = m !! v
                factor = eq !! v
                fact_eq = map (/ factor) eq
            in if factor == 0 then error "Factor 0"
               else
                 aux (v + 1) (zipWith (\i eq_i ->
                    if i == v then fact_eq
                    else let factor_i = eq_i !! v
                         in if factor_i /= 0 then zipWith (-) eq_i (map (*factor_i) fact_eq)
                         else eq_i
                    ) [0..] m)


factorize :: Int -> [Int]
factorize = aux 2
  where
    aux f n
        | f * f > n = [n]
        | n `rem` f == 0 = f : aux f (n `div` f)
        | otherwise = aux (f+1) n


divisors :: Int -> [Int]
divisors n = nub $ map product $ subsequences (factorize n)


hailPairs :: [a] -> [(a, a)]
hailPairs [] = []
hailPairs (h:hs) = ((h,) <$> hs) ++ hailPairs hs


coordSpeed :: [(Int, Int)] -> [(Int, Int)]
coordSpeed hails =
    let repeatedSpeeds = filter ((>1) . length). groupBy ((==) `on` snd) . sortBy (compare `on` snd) $ hails
        posPairs = map (\l -> (map absDiff (hailPairs (map fst l)), snd (head l))) repeatedSpeeds
    in map (first (foldl1 gcd)) posPairs
  where
    absDiff (a,b) = abs (a - b)


inters :: Eq a => [[a]] -> a
inters [] = error "inters : no coincidence"
inters [[a]] = a
inters (l1:l2:ls) = case l1 of
    []  -> error "inters: no coincidence"
    _   -> inters (intersect l1 l2 : ls)
inters _ = error "inters : no coincidence"


solveP2 :: [Hail] -> Integer
solveP2 hails = numerator $ px + py + pz
  where
    repeats coordFun =
        let accessF = fromInteger . numerator . coordFun
        in sort . coordSpeed . map (bimap accessF accessF) $ hails

    estimateSpeed =
            repeats
        >>> map (\(pos, sp) -> concatMap (\d -> [sp + d, sp - d]) (divisors pos))
        >>> inters
        >>> toRational

    sx = estimateSpeed fst3
    sy = estimateSpeed snd3
    sz = estimateSpeed thd3

    (px, py, pz) =
        let sameXSp =
                sortBy (compare `on` (fst3 . snd)) hails
              & groupBy ((==) `on` (fst3 . snd))
              & filter ((>1) . length)
              & head
        in case sort sameXSp of
        ((x1, y1, z1), (sx1, sy1, sz1)) : ((x2, y2, _z2), (_sx2, sy2, _sz2)) : _ ->
            let steps = abs (x1 - x2) / (sx - sx1)
                (src, spSrc) = min (y1, sy1) (y2, sy2)
                (dst, spDst) = max (y1, sy1) (y2, sy2)
                t0 = (dst - src - steps * sy + spDst * steps) / (spSrc - spDst)
            in (x1 + sx1 * t0 - sx * t0
               ,y1 + sy1 * t0 - sy * t0
               ,z1 + sz1 * t0 - sz * t0)
        _ -> error "No repeats"


solveP1 :: [Hail] -> Int
solveP1 = length . filter cross . hailPairs
  where
    high = 400000000000000
    low  = 200000000000000

    toMatrix ((p1x, p1y, _p1z), (s1x, s1y, _s1z)) ((p2x, p2y, _p2z), (s2x, s2y, _s2z)) =
        [[s1x, -s2x, p2x - p1x], [s1y, -s2y, p2y - p1y]]
    cross (h1, h2)
        | parallel h1 h2 = False
        | otherwise = case gauss (toMatrix h1 h2) of
            [t1, t2] ->
                let ((px, py, _pz), (sx, sy, _sz)) = h1
                in t1 >= 0 && t2 >= 0
                && px + sx * t1 >= low && px + sx * t1 <= high
                && py + sy * t1 >= low && py + sy * t1 <= high
            _ -> error "vector changed size"
    parallel (_, (s1x, s1y, _s1z))  (_, (s2x, s2y, _s2z)) =
        s1x / s1y == s2x / s2y


hailStonesP :: Parser [Hail]
hailStonesP = hailStoneP `sepEndBy1` spaces
  where
    hailStoneP = do
        pos <- (toInteger <$> intP) `sepBy1` commaSepP
        speed <- spaces >> char '@' >> spaces >> (toInteger <$> intP) `sepEndBy1` commaSepP
        case (pos, speed) of
            ([x, y, z], [dx, dy, dz]) -> pure ((x % 1, y % 1, z % 1), (dx % 1, dy % 1, dz % 1))
            _ -> unexpected "Too many coordinates"


main :: IO ()
main = applyInput hailStonesP solveP1 solveP2
