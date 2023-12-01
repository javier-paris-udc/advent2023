module Main where

import AoC                (applyInput)
import Data.Char          (digitToInt, isDigit)
import Data.List          (find, isPrefixOf)
import Control.Category   ((>>>))
import Text.Parsec        (alphaNum, many1, sepEndBy1, spaces)
import Text.Parsec.String (Parser)


nums :: [(String, Int)]
nums = [("one", 1)
       ,("two", 2)
       ,("three", 3)
       ,("four", 4)
       ,("five", 5)
       ,("six", 6)
       ,("seven", 7)
       ,("eight", 8)
       ,("nine", 9)
       ]


letterNumsToInts :: String -> [Int]
letterNumsToInts [] = []
letterNumsToInts str@(c:cs)
    | isDigit c = digitToInt c : letterNumsToInts cs
    | otherwise = case find (\(s, _) -> s `isPrefixOf` str) nums of
                        Nothing -> letterNumsToInts cs
                        Just (_, n) -> n : letterNumsToInts cs


solveP2 :: [String] -> Int
solveP2 = fmap letterNumsToInts
      >>> fmap (\n -> head n*10 + last n)
      >>> sum


solveP1 :: [String] -> Int
solveP1 = fmap (filter isDigit)
      >>> fmap (fmap digitToInt)
      >>> fmap (\n -> head n*10 + last n)
      >>> sum


linesP :: Parser [String]
linesP = many1 alphaNum `sepEndBy1` spaces


main :: IO ()
main = applyInput linesP solveP1 solveP2