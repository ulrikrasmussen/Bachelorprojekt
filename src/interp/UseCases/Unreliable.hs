module Main() where

import Aux
import Language
import Interpreter


mClasses = [ ("Server", "UseCases/Server.join")
                 , ("Sensor", "UseCases/Sensor.join") ]

machines = [ ("Server", "Server")
           , ("Sensor_A", "Sensor")
           , ("Sensor_B", "Sensor")
           , ("Sensor_C", "Sensor")
           , ("Sensor_D", "Sensor")
           ]

api = []

tempFun :: Int -> Double -> Double -> Int -> Int
tempFun avg freq amp time =
  avg + (round $ amp * sin(2*pi * fromIntegral time * freq))

readTemp t (MsgA _ [machineId, VarE k]) =
  let temp = case fromJoin machineId of
               "Sensor_A" -> tempFun 20 0.0012 10 t
               "Sensor_B" -> tempFun 20 0.0010 3 t
               "Sensor_C" -> tempFun 20 0.0001 2 t
               "Sensor_D" -> tempFun 20 0.0009 1 t
               _ -> error $ "Unknown sensor: " ++ fromJoin machineId
  in ([], [MsgA k [toJoin (temp :: Int)]])

jPrint t (MsgA _ [jStr, VarE k]) = ([OutMessage t (fromJoin jStr)],[MsgA k []])

events = ([
           EvLinkUp "Server" "Sensor_A"
         , EvLinkUp "Server" "Sensor_B"
         , EvLinkUp "Server" "Sensor_C"
         ]:(cycle [[]]))
         +&+ (linkUpProb 42 "Server" "Sensor_D" 0.9999999999)
         +&+ (mkSpecial 1 "readTemp" readTemp)
         +&+ (mkSpecial 1 "print" jPrint)

main = stdJoinMain api machines mClasses defaultConfig{breakAt = Just 10000} events
