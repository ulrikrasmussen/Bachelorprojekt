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

openJF f = do res <- parseFromFile program f
              case res of
                 Left e -> error $ show e
                 Right p -> return p

joinProgs (Proc as) (Proc as') = Proc $ as ++ as'

run conf fs = do
  progs <- mapM openJF fs
  let prog = foldl1 joinProgs progs
  ctxs <- runInterpreter conf (desugar prog)
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

helloWorldConfig = defaultConfig
 { apiMap = M.fromList [("print", jPrint)]
 }

jPrint :: Atom -> IO [Atom]
jPrint (MsgA _ [xs, VarE k]) = do
 putStrLn . map toChar . toList $ xs
 return [(MsgA k [])]
 where toList :: Expr -> [Expr]
       toList (ConE "Nil" []) = []
       toList (ConE "Cons" [x,xs]) = x:toList xs

       toChar :: Expr -> Char
       toChar x = let n = toInt x+64
                   in if n == 64 then ' '
                                 else toEnum n

       toInt :: Expr -> Int
       toInt (ConE "Z" []) = 0
       toInt (ConE "S" [x]) = 1 + toInt x

main = do
  (fs, conf) <- parseArgs helloWorldConfig [] <$> getArgs
  run conf fs
