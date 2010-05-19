module Main() where

import Aux
import Language
import Interpreter


mClasses = [ ("Single", "UseCases/Single.join")]

machines = [ ("Single", "Single")
           ]

api = []

jPrint t (MsgA _ [jStr, VarE k]) = ([OutMessage t (fromJoin jStr)],[MsgA k []])

events = (mkSpecial 1 "print" jPrint)

main = stdJoinMain api machines mClasses defaultConfig{breakAt = Just 1000} events
