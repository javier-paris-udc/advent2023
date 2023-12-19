{-# LANGUAGE RecordWildCards #-}
module Main where

import           AoC                 (applyInput, fst3, intP)
import           Data.HashMap.Strict (HashMap, (!?))
import qualified Data.HashMap.Strict as Map
import           Data.List           (find)
import           Text.Parsec         (between, char, choice, letter, many1, sepBy1, sepEndBy1, spaces, string, try)
import           Text.Parsec.String  (Parser)


data Part = Part
    { x :: Int
    , m :: Int
    , a :: Int
    , s :: Int} deriving (Show, Eq)

type Cond = (Char, Ordering, Int)
type Rule = [(Part -> Bool, Maybe Cond, String)]


aDistribution :: HashMap String Rule -> String -> HashMap Char (Int, Int) -> Int
aDistribution ruleMap rule catMap
    | rule == "A" = cases catMap
    | rule == "R" = 0
    | otherwise = case ruleMap !? rule of
        Nothing -> error "aDistribution: no rule"
        Just rules -> ruleChances catMap rules
  where
    ruleChances _partConf [] = 0
    ruleChances partConf ((_, Nothing, dst) : _) = aDistribution ruleMap dst partConf
    ruleChances partConf ((_, Just (c, ord, limit), dst) : rest) =
        case partConf !? c of
            Nothing -> error "ruleChances: no bound"
            Just (low, high)
                | (ord == GT && high < limit) || (ord == LT && low > limit) ->
                    ruleChances partConf rest
                | (ord == GT && low > limit) || (ord == LT && high < limit) ->
                    aDistribution ruleMap dst partConf
                | ord == LT ->
                      aDistribution ruleMap dst (Map.insert c (low, limit -1) partConf)
                    + ruleChances (Map.insert c (limit, high) partConf) rest
                | otherwise ->
                      aDistribution ruleMap dst (Map.insert c (limit + 1, high) partConf)
                    + ruleChances (Map.insert c (low, limit) partConf) rest

    cases :: HashMap Char (Int, Int) -> Int
    cases = product . map (\(a, b) -> b - a + 1) . Map.elems


applyRules :: HashMap String Rule -> Part -> Bool
applyRules ruleMap part = aux "in"
  where
    aux rule
        | rule == "A" = True
        | rule == "R" = False
        | otherwise = case ruleMap !? rule >>= find (($ part) . fst3) of
            Nothing -> error "applyRules : no rule"
            Just (_, _, dst) -> aux dst


solveP2 :: (HashMap String Rule, [Part]) -> Int
solveP2 (wf, _) = aDistribution wf "in" iniConf
  where
    iniConf = Map.fromList [('x', (1, 4000)), ('m', (1, 4000)), ('a', (1, 4000)), ('s', (1, 4000))]


solveP1 :: (HashMap String Rule, [Part]) -> Int
solveP1 (wf, parts) = sum $ map addFields $ filter (applyRules wf) parts
  where
    addFields p = x p + m p + a p + s p


rulesP :: Parser (HashMap String Rule, [Part])
rulesP = do
    works <- workFlowP `sepEndBy1` spaces
    parts <- partP `sepEndBy1` spaces
    pure (Map.fromList works, parts)
  where
    workFlowP = do
        name <- many1 letter
        conds <- between (char '{') (char '}') (ruleP `sepBy1` char ',')
        pure (name, conds)

    ruleP = choice [try condRuleP, lastRuleP]

    condRuleP = do
        (field, c) <- choice
            [ (x, ) <$> char 'x'
            , (m, ) <$> char 'm'
            , (a, ) <$> char 'a'
            , (s, ) <$> char 's'
            ]
        (comp, ord, n) <- choice
            [ char '<' >> intP >>= (\n -> pure ((<n) . field, LT, n))
            , char '>' >> intP >>= (\n -> pure ((>n) . field, GT, n))
            ]
        dst <- char ':' >> many1 letter
        pure (comp, Just (c, ord, n), dst)

    lastRuleP = many1 letter >>= (\dst -> pure (const True, Nothing, dst))

    partP = between (char '{') (char '}') partFieldsP

    partFieldsP = do
        x <- string "x=" >> intP
        m <- string ",m=" >> intP
        a <- string ",a=" >> intP
        s <- string ",s=" >> intP
        pure $ Part { .. }


main :: IO ()
main = applyInput rulesP solveP1 solveP2
