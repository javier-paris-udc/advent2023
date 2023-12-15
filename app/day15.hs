module Main where

import           AoC                 (applyInput, commaSepP, intP)
import           Data.Char           (ord)
import qualified Data.HashMap.Strict as Map
import           Data.List           (foldl')
import           Control.Monad       (void)
import           Text.Parsec         (char, choice, letter, many1, noneOf, runParser, sepEndBy1, space)
import           Text.Parsec.String  (Parser)


data Instruction
    = Add String Int
    | Rem String
    deriving (Show, Eq)


hash :: String -> Int
hash = foldl' doHash 0
  where
    doHash n c = (n + ord c) * 17 `mod` 256


doInstructions :: [Instruction] -> Int
doInstructions = focalPow . foldl' doInst iniMap
  where
    iniMap = Map.fromList ((, []) <$> [0 .. 255])

    doInst m (Add label lens) =
        Map.adjust (insert label lens) (hash label) m
    doInst m (Rem label) =
        Map.adjust (filter ((/=label) . fst)) (hash label) m

    insert label lens [] = [(label, lens)]
    insert newLabel newLens ((label, lens) : ls)
        | newLabel == label = (label, newLens) : ls
        | otherwise         = (label, lens) : insert newLabel newLens ls

    focalPow = Map.foldlWithKey' (\fp box lenses -> fp + (box + 1) * lensPow lenses) 0

    lensPow = fst . foldl' (\(s, i) (_, lens) -> (s + i * lens, i + 1)) (0, 1)


solveP2 :: [String] -> Int
solveP2 = doInstructions . map toInstruction
  where
    toInstruction s =
        case runParser instructionP () "" s of
            Left err -> error $ "solveP2: Parse failed - " ++ show err
            Right i  -> i

    instructionP = do
        label <- many1 letter
        choice [ char '=' >> Add label <$> intP
               , char '-' >> pure (Rem label)]


solveP1 :: [String] -> Int
solveP1 = sum . map hash


initP :: Parser [String]
initP = many1 (noneOf ", \t\n") `sepEndBy1` choice [void (many1 space), commaSepP]


main :: IO ()
main = applyInput initP solveP1 solveP2
