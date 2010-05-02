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
import Control.Concurrent
import Control.Concurrent.MVar

openJF f = do res <- parseFromFile program f
              case res of
                 Left e -> error $ show e
                 Right p -> return p

joinProgs (Proc as) (Proc as') = Proc $ as ++ as'

run conf fs = do
  progs <- mapM openJF fs
  let prog = foldl1 joinProgs progs
  ctxs <- runInterpreter conf (desugar prog)
  mapM_ putStrLn . intersperse "----------" $ map show ctxs
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
  (manips, apiMap) <- initApi [
      (return ([], M.fromList [("print", jPrint)]))
    , initTimeout
    , arithmetic
    ]
  run conf{manipulators = manips, apiMap = apiMap} fs

initApi :: [IO ([Manipulator], ApiMap)] -> IO ([Manipulator], ApiMap)
initApi apiFuns = do
  (manips, maps) <- (liftM unzip) (sequence apiFuns)
  return (concat manips, M.unions maps)

initTimeout :: IO ([Manipulator], ApiMap)
initTimeout = do
  reportMv <- newEmptyMVar :: IO (MVar [Atom])
  return ([checkTimeout reportMv], M.fromList [("sleep", registerTimeout reportMv)])
  where
    registerTimeout :: MVar [Atom] -> Atom -> IO [Atom]
    registerTimeout repMv (MsgA _ ((IntE muS):(VarE cont):_)) = 
      forkIO (threadDelay muS >> putMVar repMv [(MsgA cont [])]) >> return []
    checkTimeout :: MVar [Atom] -> IO [Atom]
    checkTimeout repMv = tryTakeMVar repMv >>= (maybe [] id >>> return)

jPrint :: Atom -> IO [Atom]
jPrint (MsgA _ [xs, VarE k]) = do
  putStrLn $ show xs
  return [MsgA k []]

arithmetic = return ([], M.fromList [
    ("add", jAdd)
  , ("mult", jMult)
  , ("div", jDiv)
  ])
  where
    jAdd (MsgA _ ((IntE op1):(IntE op2):(VarE k):[])) = return [MsgA k [IntE $ op1 + op2]]
    jMult (MsgA _ ((IntE op1):(IntE op2):(VarE k):[])) = return [MsgA k [IntE $ op1 * op2]]
    jDiv (MsgA _ ((IntE op1):(IntE op2):(VarE k):[])) = return [MsgA k [IntE $ op1 `div` op2]]
  
