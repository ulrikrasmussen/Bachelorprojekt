module Main() where

import Aux( stdJoinMain, defaultConfig )

machineClasses = [ ("Server", "UseCases/Server.join")
                 , ("Sensor", "UseCases/Sensor.join") ]

machines = [ ("Server", "Server")
           , ("Sensor_A", "Sensor")
           , ("Sensor_B", "Sensor")
           , ("Sensor_C", "Sensor")
           , ("Sensor_D", "Sensor")
           ]

comEdges = [ ("Server", "Sensor_A", 1.0)
           , ("Server", "Sensor_B", 1.0)
           , ("Server", "Sensor_C", 1.0)
           , ("Server", "Sensor_D", 0.8)
           ]

api = ([], [])

main = stdJoinMain api machines machineClasses comEdges defaultConfig
