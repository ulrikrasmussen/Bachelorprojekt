module Main() where

import Aux

machineClasses = []

machines = [ ("A", "Default") ]
comEdges = [ ]

api = ([], [])

state = GS {
   eventLog = []
 , outLog = []
 , comGraph = mkUniGraph (fst . unzip $ machines) comEdges
}

main = stdJoinMain api machines machineClasses defaultConfig state
