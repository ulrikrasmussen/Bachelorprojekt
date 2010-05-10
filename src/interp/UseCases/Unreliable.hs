module Main() where

import Aux( stdJoinMain, defaultConfig )

machineClasses = [ ("ClassA", "UseCases/A.join")
                 , ("ClassB", "UseCases/B.join") ]

machines = [ ("Server", "ClassA")
           , ("Sensor_A", "ClassB") ]

comEdges = [  ("Server", "Sensor_A", 1.0) ]

api = ([], [])

main = stdJoinMain api machines machineClasses comEdges defaultConfig
