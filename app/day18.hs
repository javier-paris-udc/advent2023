module Main where


import AoC                (applyInput, intP)
import Data.Char          (digitToInt)
import Data.List          (foldl', sort)
import Text.Parsec        (between, char, choice, hexDigit, many1, sepEndBy1, spaces, string)
import Text.Parsec.String (Parser)


data Dir = U | D | L | R deriving (Show, Eq)
type Instr = (Dir, Int)
type Coord = (Int, Int)

data Line = V Int Int Int | H Int Int Int deriving (Show, Eq, Ord)


followLine :: Coord -> [Instr] -> [Line]
followLine c instrs = snd $ foldl' doInstr (c, []) instrs
  where
    doInstr ((x, y), acc) (U, n) = ((x - n, y), V y (x - n) x : acc)
    doInstr ((x, y), acc) (D, n) = ((x + n, y), V y x (x + n) : acc)
    doInstr ((x, y), acc) (L, n) = ((x, y - n), H x (y - n) y : acc)
    doInstr ((x, y), acc) (R, n) = ((x, y + n), H x y (y + n) : acc)


twoSide :: [Line] -> [(Int, Int, Int, Int, Int)]
twoSide [] = error "empty line list"
twoSide lns@(l:ls) = case l of
    H {} ->
        aux (lns ++ [l])
    V {} ->
        aux (last ls : lns)
  where
    aux []  = []
    aux [_] = []
    aux [_, _] = error "twoSide : Two element list"
    aux ((H r0 y0i y0e) : (V c xi xe) : (H r1 y1i y1e) : rest) =
        adjust c xi xe xi xe [(r0, y0i, y0e), (r1, y1i, y1e)] : aux (H r1 y1i y1e : rest)
    aux _ = error "twoSide: wrong order"

    adjust c xil xel xir xer [] = (c, xil, xel, xir, xer)
    adjust c xil xel xir xer ((r, yi, ye):hs)
        | r == xil && ye == c = adjust c (xil + 1) xel xir xer hs
        | r == xel && ye == c = adjust c xil (xel - 1) xir xer hs
        | r == xil && yi == c = adjust c xil xel (xir + 1) xer hs
        | r == xel && yi == c = adjust c xil xel xir (xer - 1) hs
        | otherwise = error "bad adjust"


solve :: [Line] -> Int
solve lns = (+len lns) $ fst $ foldl' doLine (0, []) sortedLines
  where
    sortedLines = sort $ twoSide lns

    doLine (area, lls) (cl, stl, endl, str, endr) =
        let (newArea, newlls) = overlap 0 [] lls cl stl endl str endr
        in (area + newArea, newlls)

    overlap area acc [] col _stl _endl str endr = (area, reverse ((col, str, endr) : acc))
    overlap area acc lls@(l0@(col0, st0, end0):rls) col stl endl str endr
        | end0 < stl  = overlap area (l0 : acc) rls col stl endl str endr
        | endl < st0  = (area, reverse acc ++ (col, str, endr) : lls)
        | st0  < stl  = overlap area ((col0, st0, stl - 1) : acc) ((col0, stl, end0) : rls) col stl endl str endr
        | stl  < st0  = overlap area ((col, str, st0 - 1) : acc) lls col st0 endl st0 endr
        | end0 < endl =
            let newArea = area + (end0 - st0 + 1) * (col - col0 - 1)
            in overlap newArea acc rls col (end0 + 1) endl (end0 + 1) endr
        | end0 == endl =
            let newArea = area + (end0 - st0 + 1) * (col - col0 - 1)
            in (newArea, reverse acc ++ rls)
        | otherwise = -- end0 > endl
            let newArea = area + (endl - st0 + 1) * (col - col0 - 1)
            in (newArea, reverse acc ++ (col0, endl + 1, end0) : rls)

    len [] = 0
    len (V _ st end : rest) = abs (st - end) + len rest
    len (H _ st end : rest) = abs (st - end) + len rest


solveP2 :: [Instr] -> Int
solveP2 = solve . followLine (0,0)


solveP1 :: [Instr] -> Int
solveP1 = solve . followLine (0,0)


instrsP :: Parser ([Instr], [Instr])
instrsP = unzip <$> instrP `sepEndBy1` spaces
  where
    instrP = do
        dir <- choice
            [ char 'U' >> pure U
            , char 'D' >> pure D
            , char 'L' >> pure L
            , char 'R' >> pure R
            ]
        n <- spaces >> intP
        color <- spaces >> between (string "(#") (char ')') hexP
        let dir2 = case color `mod` 16 of
                0 -> R
                1 -> D
                2 -> L
                3 -> U
                _ -> error "Bad dir"
        pure ((dir, n), (dir2, color `div` 16))

    hexP = do
        hexDigits <- many1 hexDigit
        pure $ foldl' (\n d -> n * 16 + digitToInt d) 0 hexDigits


main :: IO ()
main = applyInput instrsP (solveP1 . fst) (solveP2 . snd)
