{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Desugar(desugar) where

import Language

import Control.Monad.State
import Control.Applicative

instance Applicative (State s) where
    pure = return
    (<*>) = ap

newtype DesugarM a = D (State [String] a)
    deriving (Monad, Functor, Applicative)

desugar :: Proc -> Proc
desugar (Proc as) = Proc $ runDesugar (mapM desAtom as)

-- We mangle implicit continuations based on the name of the synchronous function name.
-- As long as the function name is in scope, so is the continuation name.
contName f = f ++ "?cont"

getFresh :: Int -> DesugarM [String]
getFresh n = D $ do (names, names') <- gets (splitAt n)
                    put names'
                    return names

runDesugar :: DesugarM a -> a
runDesugar (D s) = evalState s $ map ('?':) $ tail supply
  where supply = "":[ m++[l] | m <- supply, l <- ['a' .. 'z'] ]

type Continuation = String

-- Desugar synchronous expressions
desExp :: SExpr -> Continuation -> DesugarM Atom
desExp (VarS v) k = return $ MsgA k [VarE v]
desExp (ZeroS) k = return $ MsgA k [ZeroE]
desExp (SuccS e) k =
  do [l, v] <- getFresh 2
     e' <- desExp e l
     return $ DefA [ReactionD [VarJ l [VarP v]]
                              (Proc [MsgA k [SuccE (VarE v)]])]
                   (Proc [e'])
desExp (CallS v es) k =
  do chans <- getFresh $ length es
     vars <- getFresh $ length es
     let joins = zipWith (\chan var -> VarJ chan [VarP var]) chans vars
     es' <- zipWithM desExp es chans
     return $ DefA [ReactionD joins
                              (Proc [MsgA v (map VarE vars ++ [VarE k])])]
                   (Proc $ es')

desInstr :: Maybe Continuation -> [Instr] -> DesugarM [Atom]
desInstr mk [] = maybe (return [InertA])
                       (\k -> return [MsgA k []]) mk
desInstr mk ((LetI pat e):is) =
  do is' <- desInstr mk is
     [k] <- getFresh 1
     e' <- desExp e k
     return $ [DefA [ReactionD [VarJ k [pat]] (Proc is')]
                    (Proc [e'])]
desInstr mk ((RunI (Proc as)):is) = (as ++) <$> desInstr mk is

desInstr mk ((DoI f es):is) =
  do is' <- desInstr mk is
     [k, dummy] <- getFresh 2
     f' <- desExp (CallS f es) k
     return $ [DefA [ReactionD [VarJ k [VarP dummy]] (Proc is')]
                    (Proc [f'])]

desInstr mk ((MatchI e mps):is) =
  do is' <- desInstr mk is
     [l, m, var_e] <- getFresh 3
     e' <- desExp e l
     mps' <- mapM (\(pat, is) -> (,) pat <$> (Proc <$> desInstr (Just m) is)) mps
     return $ [DefA [ReactionD [VarJ l [VarP var_e]]
                               (Proc $ [MatchA (VarE var_e) mps'])
                    ,ReactionD [VarJ m []]
                               (Proc $ is')]
                    (Proc [e'])]
     
desInstr mk ((ReturnI es f):is) =
  do is' <- desInstr mk is
     chans <- getFresh $ length es
     vars <- getFresh $ length es
     es' <- zipWithM desExp es chans
     let joins = zipWith (\chan var -> VarJ chan [VarP var]) chans vars
     return [DefA [ReactionD joins
                             (Proc $ (MsgA (contName f) $ map VarE vars):is')]
                  (Proc es')]

desAtom :: Atom -> DesugarM Atom
desAtom (DefA defs (Proc as)) = DefA <$> mapM desDef defs <*> (Proc <$> mapM desAtom as)
desAtom (MatchA e mps) =
    MatchA e <$> mapM (\(pat, (Proc as)) -> (,) pat <$> (Proc <$> mapM desAtom as)) mps
desAtom (InstrA is) =
   do [k] <- getFresh 1
      is' <- desInstr (Just k) is
      return $ DefA [ReactionD [VarJ k []] (Proc $ [InertA])]
                    (Proc $ is')
desAtom a = return a

desDef :: Def -> DesugarM Def
desDef (ReactionD js (Proc as)) =
  ReactionD (map desJoin js) <$> (Proc <$> mapM desAtom as)

desJoin :: Join -> Join
desJoin (SyncJ f ps) = VarJ f (ps ++ [VarP $ contName f])
desJoin j = j