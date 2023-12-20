{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Main where

import           AoC                        (applyInput, blanksP, fst3)
import           Control.Monad              (replicateM_, unless, when)
import           Control.Monad.Loops        (untilM_)
import           Control.Monad.State.Strict (State, execState, get, gets, modify, put)
import           Data.HashMap.Strict        (HashMap, (!?))
import qualified Data.HashMap.Strict        as Map
import           Data.List                  (find)
import           Data.Sequence              (Seq ((:|>), (:<|)))
import qualified Data.Sequence              as Seq
import           Data.Maybe                 (catMaybes, isJust)
import           Text.Parsec                (char, choice, letter, many1, sepBy1, sepEndBy1, spaces, string)
import           Text.Parsec.String         (Parser)


data Module
    = Broadcaster [String]
    | FlipFlop [String] Bool
    | Conjunction [String] (HashMap String Bool)
    deriving (Show, Eq)


data NetSt = St
    { pulses  :: Seq (String, String, Bool)
    , mods    :: HashMap String Module
    , high    :: Int
    , low     :: Int
    , rxSrc   :: String
    , rxIns   :: HashMap String (Maybe Int)
    , buttons :: Int
    } deriving (Show, Eq)


doPulse :: String -> String -> Bool -> State NetSt ()
doPulse src dst pulse = do
    St { .. } <- get
    case mods !? dst of
        Nothing -> pure ()
        Just (Broadcaster dsts) ->
            mapM_ (queuePulse pulse dst) dsts
        Just (FlipFlop dsts fpV) ->
            unless pulse $ do
                put (St { mods = Map.insert dst (FlipFlop dsts (not fpV)) mods, ..})
                mapM_ (queuePulse (not fpV) dst) dsts
        Just (Conjunction dsts sts) -> do
            let newSts = Map.insert src pulse sts
            put (St { mods = Map.insert dst (Conjunction dsts newSts) mods, ..})
            when (dst == rxSrc && pulse) $ do
                case rxIns !? src of
                    Nothing -> error "unknown input"
                    Just Nothing -> put (St { rxIns = Map.insert src (Just buttons) rxIns, ..})
                    _ -> pure ()
            mapM_ (queuePulse (not (and newSts)) dst) dsts


processQueue :: State NetSt ()
processQueue = do
    q <- gets pulses
    case q of
        Seq.Empty -> pure ()
        (src, dst, pulse) :<| q_rest -> do
            modify (\s -> s { pulses = q_rest })
            doPulse src dst pulse
            processQueue


queuePulse :: Bool -> String -> String -> State NetSt ()
queuePulse True  src dst = modify (\s -> s { pulses = pulses s :|> (src, dst, True ), high = high s + 1 })
queuePulse False src dst = modify (\s -> s { pulses = pulses s :|> (src, dst, False), low  = low  s + 1 })


incPulses :: Bool -> Int -> State NetSt ()
incPulses True  n = modify (\s -> s { high = high s + n })
incPulses False n = modify (\s -> s { low  = low  s + n })


pushButton :: State NetSt ()
pushButton = do
    queuePulse False "button" "broadcaster"
    modify (\s -> s { buttons = buttons s + 1 })
    processQueue


rxInsCycled :: State NetSt Bool
rxInsCycled = do
    rxIn <- gets rxIns
    pure $ all isJust rxIn


iniSt :: HashMap String Module -> NetSt
iniSt m = St { pulses = Seq.empty, mods = m, high = 0, low = 0, rxSrc = rxSrc, rxIns = rxIns0, buttons = 0 }
  where
    rxSrc = case find (isRxSrc . snd) (Map.toList m) of
        Nothing -> error "No source for rx"
        Just (src, _) -> src

    rxIns0 = case m !? rxSrc of
        Nothing -> Map.empty
        Just (Conjunction _ insSrc) -> Map.map (const Nothing) insSrc
        _ -> error "rx is not a conjunction"

    isRxSrc (Conjunction dsts _) = "rx" `elem` dsts
    isRxSrc _ = False


solveP2 :: HashMap String Module -> Int
solveP2 m = getRepeat $ execState (untilM_ pushButton rxInsCycled) (iniSt m)
  where
    getRepeat st = foldl1 lcm $ catMaybes $ Map.elems (rxIns st)


solveP1 :: HashMap String Module -> Int
solveP1 m = multPulses $ execState (replicateM_ 1000 pushButton) (iniSt m)
  where
    multPulses s = high s * low s


modulesP :: Parser (HashMap String Module)
modulesP = do
    modList <- modP `sepEndBy1` spaces
    pure $ Map.fromList $ buildModules modList
  where
    modP = do
        (name, modType) <- choice
            [ string "broadcaster" >> pure ("broadcaster", "Broadcaster")
            , (, "FlipFlop") <$> (char '%' >> many1 letter)
            , (, "Conjunction") <$> (char '&' >> many1 letter)]
        spaces >> string "->" >> spaces
        dsts <- many1 letter `sepBy1` (char ',' >> blanksP)
        pure (name, modType, dsts)

    buildModules modL = map (buildModule modL) modL

    buildModule modL m = case m of
        (name, "Broadcaster", dsts) -> (name, Broadcaster dsts)
        (name, "FlipFlop", dsts)    -> (name, FlipFlop dsts False)
        (name, "Conjunction", dsts) -> (name, Conjunction dsts (getSrcs name modL))
        _ -> error "Bad type"

    getSrcs name = Map.fromList . map ((, False) . fst3) . filter (\(_, _, dsts) -> name `elem` dsts)


main :: IO ()
main = applyInput modulesP solveP1 solveP2
