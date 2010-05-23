module Main() where

import Aux
import Language
import Interpreter

-- Currently, this file test a time lock.

mClasses = [ ("Simple1", "UseCases/Simple1.join")
           , ("Simple2", "UseCases/Simple2.join")]

machines = [ ("Simple1", "Simple1")
           {-, ("Simple2", "Simple2")-}]

api = []

jPrint t (MsgA _ [jStr, VarE k]) = ([OutMessage t (fromJoin jStr)],[MsgA k []])

events = (mkSpecial 1 "print" jPrint)
         +&+ ([EvLinkUp "Simple1" "Simple2"]:(cycle [[]]))

main = stdJoinMain api machines mClasses defaultConfig{breakAtTime = Just 100} events
