module Main() where

import Aux( stdJoinMain, defaultConfig )

machineClasses = [("Collector", "collector.join")]

machines = [ ("Server", "Collector")
           , ("Sensor_A", "Sensor")
           , ("Sensor_B", "Sensor")
           , ("Sensor_C", "Sensor")
           , ("Sensor_D", "Sensor")
           ]
comEdges = [ ("Sensor1", "Server", 0.9)
           , ("Sensor2", "Server", 0.9)
           , ("Sensor3", "Server", 0.9)
           , ("Sensor4", "Server", 0.9)
           ]

api = ([], [
  ("readTemp", undefined)
  ])

main = stdJoinMain api machines comEdges defaultConfig
