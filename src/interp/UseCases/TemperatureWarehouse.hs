module Main() where

import Aux( stdJoinMain, defaultConfig )

machineClasses = [("Collector", "path/to/collector.join")]

machines = [ ("Server", "Collector")
           , ("Sensor1", "Sensor")
           , ("Sensor2", "Sensor")
           , ("Sensor3", "Sensor")
           , ("Sensor4", "Sensor")
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
