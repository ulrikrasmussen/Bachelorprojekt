{-# LANGUAGE FlexibleInstances #-}
-- vim:set foldmethod=marker foldmarker=--{,--}:
module JoinApi(integerArith
              ,jPrint
              ,initApi
              ,initTimeout
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

--{ Conversion functions
class JoinValue a where
 fromJoin :: Expr -> a
 toJoin :: a -> Expr

instance JoinValue Bool where
 fromJoin (ConE "True" []) = True
 fromJoin (ConE "Fals" []) = False
 fromJoin x = error $ "Not a join boolean: '" ++ show x ++ "'"

 toJoin True = ConE "True" []
 toJoin False = ConE "False" []

instance JoinValue Int where
 fromJoin (IntE i) = i
 fromJoin x = error $ "Not a join integer: '" ++ show x ++ "'"

 toJoin = IntE

instance (JoinValue a) => JoinValue [a] where
 fromJoin (ConE "Nil" []) = []
 fromJoin (ConE "Cons" [x, xs]) = fromJoin x : fromJoin xs
 fromJoin x = error $ "Not a join list: '" ++ show x ++ "'"

 toJoin [] = ConE "Nil" []
 toJoin (x:xs) = ConE "Cons" [toJoin x, toJoin xs]

instance JoinValue Char where
 fromJoin = toEnum . fromJoin
 toJoin = toJoin . fromEnum
--}

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


jPrint :: Atom -> IO [Atom]
jPrint (MsgA _ [jStr, VarE k]) = do
 putStr . fromJoin $ jStr
 return [MsgA k []]
