module Main() where

import Aux( stdJoinMain, defaultConfig )

machineClasses = []

machines = [ ("A", "Default") ]
comEdges = [ ]

api = ([], [
  ("readTemp", undefined)
  ])

main = stdJoinMain api machines machineClasses comEdges defaultConfig
