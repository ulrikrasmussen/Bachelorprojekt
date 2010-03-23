module Main() where

import Parser
import Language
import Desugar
import Interpreter
import Text.ParserCombinators.Parsec
import System

parseString ::  [Char] -> Proc
parseString str =
    case runParser program () "" str of
        Left e -> error $ show e
        Right p -> p

testProgram = parseString "def x() |> y() or y() |> x() in x() & def x() |> y() in 0 & x()"

openJF f = do res <- parseFromFile program f
              case res of
                 Left e -> error $ show e
                 Right p -> return p

run f = do
  testProg <- openJF $ f
  putStrLn . show $ runInterpreter (desugar testProg)

main = do
  [f] <- getArgs
  run f
