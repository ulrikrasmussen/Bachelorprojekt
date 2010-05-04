{-# LANGUAGE FlexibleInstances #-}
-- vim:set foldmethod=marker foldmarker=--{,--}:
module JoinApi(integerArith
              ,jPrint
              ,initApi
              ,initTimeout
              ,initNameServer
              ,output
              ,ApiMap
              ,Manipulator) where

import Control.Applicative
import Control.Monad
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.MVar
import Language

import qualified Data.Map as M

type ApiMap      = M.Map String (Atom -> IO [Atom])
type Manipulator = IO [Atom]

integerArith :: ([Manipulator], ApiMap)
integerArith = ([], M.fromList [
    ("add", jAdd)
  , ("mult", jMult)
  , ("div", jDiv)
  , ("leq", jLeq)
  ])
  where
    jAdd (MsgA _ [IntE op1, IntE op2, VarE k]) = return [MsgA k [IntE $ op1 + op2]]
    jMult (MsgA _ [IntE op1, IntE op2, VarE k]) = return [MsgA k [IntE $ op1 * op2]]
    jDiv (MsgA _ [IntE op1, IntE op2, VarE k]) = return [MsgA k [IntE $ op1 `div` op2]]
    jLeq (MsgA _ [IntE op1, IntE op2, VarE k]) = return [MsgA k [toJoin $ op1 <= op2]]

output = ([], M.fromList [
   ("print", jPrint)
  ])

initApi :: [([Manipulator], ApiMap)] -> ([Manipulator], ApiMap)
initApi xs = let (manips, maps) = unzip xs
              in (concat manips, M.unions maps)

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


initNameServer :: IO ([Manipulator], ApiMap)
initNameServer = do
  nsVar <- newEmptyMVar :: IO (MVar (M.Map String Expr))
  nsVar `putMVar` M.empty
  return ([], M.fromList([("search", jSearch nsVar), ("register", jRegister nsVar)]))
  where
    jSearch nsVar (MsgA _ [name, VarE k]) =
      withMVar nsVar $ \m -> return [MsgA k [toJoin $ M.lookup (fromJoin name) m]]

    jRegister nsVar (MsgA _ [name, expr, VarE k]) =
      modifyMVar nsVar $ \m ->
        let m' = M.insert (fromJoin name) expr m
            msg = MsgA k []
         in return (m', [msg])

jPrint :: Atom -> IO [Atom]
jPrint (MsgA _ [jStr, VarE k]) = do
 putStr . fromJoin $ jStr
 return [MsgA k []]