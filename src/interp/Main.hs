module Main() where

import Parser
import Language
import Desugar
import Interpreter
import Text.ParserCombinators.Parsec
import System
import Control.Applicative

openJF f = do res <- parseFromFile program f
              case res of
                 Left e -> error $ show e
                 Right p -> return p

joinProgs (Proc as) (Proc as') = Proc $ as ++ as'

run conf fs = do
  progs <- mapM openJF fs
  let prog = foldl1 joinProgs progs
  putStrLn . show =<< runInterpreter conf (desugar prog)

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
  run conf fs
