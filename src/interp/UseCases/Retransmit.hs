module Main() where

import Aux( stdJoinMain, defaultConfig )

machineClasses = [ ("Server", "UseCases/RetransmitServer.join")
                 , ("Client", "UseCases/RetransmitClient.join") 
                 ]

machines = [ ("Server", "Server")
           , ("Client", "Client")
           ]

comEdges = [ ("Server", "Client", 1.0)
           ]

api = ([], [])

main = stdJoinMain api machines machineClasses comEdges defaultConfig
