module Main() where

import Aux

machineClasses = []

machines = [ ("A", "Default") ]
comEdges = [ ]

api = ([], [
  ("readTemp", undefined)
  ])

state = GS {}

main = stdJoinMain api machines machineClasses defaultConfig state
