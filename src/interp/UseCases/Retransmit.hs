module Main() where

import Aux( stdJoinMain, defaultConfig )

machineClasses = [ ("Server", "UseCases/RetransmitServer.join")
                 , ("Client", "UseCases/RetransmitClient.join")
                 ]

machines = [ ("Server", "Server")
           , ("Client", "Client")
           ]

comEdges = [ ("Server", "Client", 0.5)
           ]

api = ([], [])

main = stdJoinMain api machines machineClasses comEdges defaultConfig
      {--
        ; let res = proxMult(Cons(7,Cons(17),Nil))
        ; do print("Server says 7*17=")
        ; do printInt(res)
        ; do print("\n")
        -}
