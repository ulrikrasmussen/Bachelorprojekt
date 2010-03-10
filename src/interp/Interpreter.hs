module Interpreter() where

data Context = Context { defs :: [Def]
                       , atms :: [Atom] }


