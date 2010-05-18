module Aux( stdJoinMain
          , defaultConfig
          , Output
          , OutputLog
          , Event
          , EventLog
          , GlobalState(..) ) where

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

openJF f = do res <- parseFromFile program f
              case res of
                 Left e -> error $ show e
                 Right p -> return p

joinProgs (Proc as) (Proc as') = Proc $ as ++ as'

defaultConfig = IC {
    runGC = True
  , gcInterval = 1
  , breakAt = Nothing
  , nondeterministic = False
  , apiMap = M.fromList []
  --, manipulators = []
  , machineClasses = M.empty
  , initialMachines = []
  --, comLinks = (M.empty)
}

parseArgs conf fs [] = (fs, conf)
parseArgs conf fs ("-n":xs) =
    parseArgs (conf {nondeterministic = True}) fs xs
parseArgs conf fs ("-b":c:xs) =
    parseArgs (conf {breakAt = Just $ read c}) fs xs
parseArgs conf fs ("-nogc":xs) =
    parseArgs (conf {runGC = False}) fs xs
parseArgs conf fs (f:xs) =
    parseArgs conf (f:fs) xs

stdJoinMain (man, api) machines mClasses cfg state = do
  let cfg' = cfg { initialMachines = machines}
                 --, comLinks = mkUniGraph (fst . unzip $ machines) comEdges }

  (fs, conf) <- parseArgs cfg' [] <$> getArgs
  timeout <- initTimeout
  ns <- initNameServer
  temp <- initTempSensor
  let (manips, apiMap) =
       initApi
         [ (man, M.fromList api)
         , output
         , ns
         , timeout
         , temp
         , integerArith
         , boolean
         ]
  -- Parse files from the command line. These are used for the "Default" machine class
  (Proc as) <- desugar . foldl joinProgs (Proc []) <$> mapM openJF fs
  -- Parse files from the given list of machine classes
  let (cls, paths) = unzip mClasses
  progs <- map ((\(Proc as) -> as) . desugar) <$> mapM openJF paths
  (output, ctxs) <- runInterpreter conf{ -- manipulators = manips
                             apiMap = apiMap
                             , machineClasses =
                                   M.fromList $ ("Default", as):(zip cls progs)
                             } state
  mapM_ putStrLn . intersperse "----------" $ map show ctxs
  putStr (intersperse "\n" output)
  return ()
