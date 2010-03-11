module Main() where

import Parser
import Language
import Interpreter
import Text.ParserCombinators.Parsec

parseString ::  [Char] -> Proc
parseString str =
    case runParser program () "" str of
        Left e -> error "could not parse"
        Right p -> p

testProgram = parseString "def x() |> y() or y() |> x() in x() & def x() |> y() in 0 & x()"

openJF f = do res <- parseFromFile program f
              case res of
                 Left e -> error "could not parse"
                 Right p -> return p

main = do
  testProg <- openJF "../examples/test2.join"
  putStrLn . show $ runInterpreter testProg
