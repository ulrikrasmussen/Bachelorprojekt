module Main() where

import Aux
import Language
import Interpreter

mClasses = [ ("Server", "UseCases/RetransmitServer.join")
                 , ("Client", "UseCases/RetransmitClient.join")
                 ]

machines = [ ("Server", "Server")
           , ("Client", "Client")
           ]

api = []

jPrint t (MsgA _ [jStr, VarE k]) = ([OutMessage t (fromJoin jStr)],[MsgA k []])

events = (mkSpecial 1 "print" jPrint)
         +&+ (atTime 0 $ linkUpProb 604 "Server" "Client" 0.5)

main = stdJoinMain api machines mClasses defaultConfig{breakAtTime = Just 100} events
