module Aux( stdJoinMain
          , defaultConfig
          , OutputLog
          , Event(..)
          , Output(..)
          , EventLog
          , GlobalState(..)
          , (+&+)
          , linkUpProb
          , mkSpecial
          , mkUniGraph) where

import Parser
import Language
import Desugar
import GlobalInterpreter
import Text.ParserCombinators.Parsec
import System
import Control.Applicative
import Data.List (intersperse)
import qualified Data.Map as M
import Control.Monad
import Control.Arrow

import JoinApi
import System.Random

openJF f = do res <- parseFromFile program f
              case res of
                 Left e -> error $ show e
                 Right p -> return p

joinProgs (Proc as) (Proc as') = Proc $ as ++ as'

defaultConfig = IC {
    runGC = True
  , gcInterval = 1
  , breakAtIter = Nothing
  , breakAtTime = Nothing
  , nondeterministic = False
  , apiMap = M.fromList []
  , machineClasses = M.empty
  , initialMachines = []
}

(+&+) = zipWith (++)

linkUpProb :: Int -> String -> String -> Double -> [[Event]]
linkUpProb seed m1 m2 p =
  linkUpProb' (mkStdGen seed) m1 m2 p
  where
    linkUpProb' rg m1 m2 p = let
      (dice, rg') = randomR (0,1) rg
      in
        [(if dice >= p then EvLinkUp m1 m2 else EvLinkDown m1 m2)]:(linkUpProb' rg' m1 m2 p)

mkSpecial n nm fun =
  [EvSpecial nm (fun n)]:(mkSpecial (n+1) nm fun)

parseArgs conf fs [] = (fs, conf)
parseArgs conf fs ("-n":xs) =
    parseArgs (conf {nondeterministic = True}) fs xs
parseArgs conf fs ("-bI":c:xs) =
    parseArgs (conf {breakAtIter = Just $ read c}) fs xs
parseArgs conf fs ("-bT":c:xs) =
    parseArgs (conf {breakAtTime = Just $ read c}) fs xs
parseArgs conf fs ("-nogc":xs) =
    parseArgs (conf {runGC = False}) fs xs
parseArgs conf fs (f:xs) =
    parseArgs conf (f:fs) xs

stdJoinMain api machines mClasses cfg events = do
  let cfg' = cfg { initialMachines = machines}
  (fs, conf) <- parseArgs cfg' [] <$> getArgs
  --timeout <- initTimeout
  ns <- initNameServer
  --temp <- initTempSensor
  let apiMap =
       initApi
         [ M.fromList api
         , ns
         , integerArith
         , boolean
         ]
  -- Parse files from the command line. These are used for the "Default" machine class
  (Proc as) <- desugar . foldl joinProgs (Proc []) <$> mapM openJF fs
  -- Parse files from the given list of machine classes
  let (cls, paths) = unzip mClasses
  progs <- map ((\(Proc as) -> as) . desugar) <$> mapM openJF paths
  (output, ctxs) <- runInterpreter conf{
                             apiMap = apiMap
                             , machineClasses =
                                   M.fromList $ ("Default", as):(zip cls progs)
                             } initialState{eventLog = events}
  --mapM_ putStrLn . intersperse "----------" $ map show ctxs
  --mapM_ putStrLn $ map show output
  return ()
