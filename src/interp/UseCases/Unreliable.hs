module Main() where

import Aux( stdJoinMain, defaultConfig )

machineClasses = [ ("ClassA", "UseCases/A.join")
                 , ("ClassB", "UseCases/B.join") ]

machines = [ ("A", "ClassA")
           , ("B", "ClassB") ]

comEdges = [  ("A", "B", 1.0) ]

api = ([], [
  ("readTemp", undefined)
  ])

main = stdJoinMain api machines machineClasses comEdges defaultConfig
