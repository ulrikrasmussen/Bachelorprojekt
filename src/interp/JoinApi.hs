{-# LANGUAGE FlexibleInstances #-}
-- vim:set foldmethod=marker foldmarker=--{,--}:
module JoinApi(integerArith
              ,jPrint
              ,initApi
              --,initTimeout
              ,initNameServer
              --,initTempSensor
              ,output
              ,boolean
              ) where

import GlobalInterpreter
import Control.Applicative
import Control.Monad
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.MVar
import Language

import qualified Data.Map as M

import Debug.Trace

--type Manipulator = IO [Atom]

boolean :: ApiMap
boolean = M.fromList [
    ("eq", exprEq)
  , ("and", exprAnd)
  ]
 where
  exprEq (MsgA _ [e1, e2, VarE k]) = return [MsgA k [toJoin (e1 == e2)]]
  exprAnd (MsgA _ [e1, e2, VarE k]) = return [MsgA k [toJoin ((fromJoin e1) && (fromJoin e2))]]

{-
initTempSensor :: IO ([Manipulator], ApiMap)
initTempSensor = do
  tV <- newMVar 0
  return ([progressTime tV],
          M.fromList [
            ("readTemp", readTemp tV)
          ])
  where
    progressTime tV = modifyMVar tV (\t -> return (t+1,[]))
    tempFun :: Int -> Double -> Double -> Int -> Int
    tempFun avg freq amp time =
      avg + (round $ amp * sin(2*pi * fromIntegral time * freq))
    readTemp tV (MsgA _ [machineId, VarE k]) = do
      t <- readMVar tV
      let temp = case fromJoin machineId of
                   "Sensor_A" -> tempFun 20 0.0012 10 t
                   "Sensor_B" -> tempFun 20 0.0010 3 t
                   "Sensor_C" -> tempFun 20 0.0001 2 t
                   "Sensor_D" -> tempFun 20 0.0009 1 t
                   _ -> error $ "Unknown sensor: " ++ fromJoin machineId
      return [MsgA k [toJoin (temp :: Int)]]
-}

integerArith :: ApiMap
integerArith = M.fromList [
    ("add", jAdd)
  , ("sub", jSub)
  , ("mult", jMult)
  , ("div", jDiv)
  , ("mod", jMod)
  , ("leq", jLeq)
  ]
  where
    jAdd (MsgA _ [IntE op1, IntE op2, VarE k]) = return [MsgA k [IntE $ op1 + op2]]
    jSub (MsgA _ [IntE op1, IntE op2, VarE k]) = return [MsgA k [toJoin . max 0 $ op1 - op2]]
    jMult (MsgA _ [IntE op1, IntE op2, VarE k]) = return [MsgA k [IntE $ op1 * op2]]
    jDiv (MsgA _ [IntE op1, IntE op2, VarE k]) = return [MsgA k [IntE $ op1 `div` op2]]
    jMod (MsgA _ [IntE op1, IntE op2, VarE k]) = return [MsgA k [toJoin $ op1 `mod` op2]]
    jLeq (MsgA _ [IntE op1, IntE op2, VarE k]) = return [MsgA k [toJoin $ op1 <= op2]]

output = M.fromList [
    ("print", jPrint)
  , ("printInt", jPrintInt)
  , ("dump", jDump)
  ]

initApi :: [ApiMap] -> (ApiMap)
initApi xs = M.unions xs

{-
initTimeout :: IO ([Manipulator], ApiMap)
initTimeout = do
  reportMv <- newEmptyMVar :: IO (MVar [Atom])
  return ([checkTimeout reportMv], M.fromList [("sleep", registerTimeout reportMv)])
  where
    registerTimeout :: MVar [Atom] -> Atom -> IO [Atom]
    registerTimeout repMv (MsgA _ ((IntE muS):(VarE cont):_)) =
      forkIO (threadDelay muS >> putMVar repMv [(MsgA cont [])]) >> return []
    checkTimeout :: MVar [Atom] -> IO [Atom]
    checkTimeout repMv = tryTakeMVar repMv >>= (maybe [] id >>> return)
    -}


initNameServer :: IO (ApiMap)
initNameServer = do
  nsVar <- newEmptyMVar :: IO (MVar (M.Map String Expr))
  nsVar `putMVar` M.empty
  return (M.fromList([("search", jSearch nsVar), ("register", jRegister nsVar)]))
  where
    jSearch nsVar (MsgA _ [name, VarE k]) =
      withMVar nsVar $ \m -> return [MsgA k [toJoin $ M.lookup (fromJoin name) m]]

    jRegister nsVar (MsgA _ [name, expr, VarE k]) = do
      putStrLn $ "registered name: " ++ (fromJoin name :: String)
      modifyMVar nsVar $ \m ->
        let m' = M.insert (fromJoin name) expr m
            msg = MsgA k []
         in return (m', [msg])

jPrint :: Atom -> IO [Atom]
jPrint (MsgA _ [jStr, VarE k]) = do
 putStr . fromJoin $ jStr
 return [MsgA k []]

jPrintInt :: Atom -> IO [Atom]
jPrintInt (MsgA _ [IntE i, VarE k]) = do
 putStr . show $ i
 return [MsgA k []]

jDump :: Atom -> IO [Atom]
jDump (MsgA _ [x, VarE k]) = do
  putStr . show $ x
  return [MsgA k []]
