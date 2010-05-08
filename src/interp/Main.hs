module Main() where

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

run conf fs = do
  progs <- mapM openJF fs
  let prog = foldl1 joinProgs progs
  ctxs <- runInterpreter conf
  --mapM_ putStrLn . intersperse "----------" $ map show ctxs
  return ()

parseArgs conf fs [] = (fs, conf)
parseArgs conf fs ("-n":xs) =
    parseArgs (conf {nondeterministic = True}) fs xs
parseArgs conf fs ("-b":c:xs) =
    parseArgs (conf {breakAt = Just $ read c}) fs xs
parseArgs conf fs ("-nogc":xs) =
    parseArgs (conf {runGC = False}) fs xs
parseArgs conf fs (f:xs) =
    parseArgs conf (f:fs) xs

main = do
  (fs, conf) <- parseArgs defaultConfig [] <$> getArgs
  timeout <- initTimeout
  ns <- initNameServer
  let (manips, apiMap) =
       initApi [
          output
        , ns
        , timeout
        , integerArith
        ]
  run conf{manipulators = manips, apiMap = apiMap} fs
