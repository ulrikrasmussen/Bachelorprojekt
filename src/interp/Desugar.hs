{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Desugar(desugar) where

import Language

import Control.Monad.State
import Control.Applicative

-- |We define a desugar monad that supplies us with fresh names.
newtype DesugarM a = D (State [String] a)
    deriving (Monad, Functor)

getFresh :: Int -> DesugarM [String]
getFresh n = D $ do (names, names') <- gets (splitAt n)
                    put names'
                    return names

runDesugar :: DesugarM a -> a
runDesugar (D s) = evalState s $ map ('?':) $ tail supply
  where supply = "":[ m++[l] | m <- supply, l <- ['a' .. 'z'] ]

desugar :: Proc -> Proc
desugar (Proc as) = Proc . concat $ runDesugar (mapM desAtom as)

-- We mangle implicit continuations based on the name of the synchronous function name.
-- As long as the function name is in scope, so is the continuation name.
contName f = f ++ "?cont"

type Continuation = String

-- Desugar synchronous expressions. To keep the code simple, we compute
-- expressions in 'chains', where each constructor or value computes any
-- subexpressions and sends its result on its continuation.
desExp :: Continuation -> SExpr -> DesugarM Atom

desExp k (VarS v) = return $ MsgA k [VarE v]
desExp k (IntS i) = return $ MsgA k [IntE i]
desExp k (ConS n []) = return $ MsgA k [ConE n []]
desExp k (ConS n es) =
  do chans <- getFresh $ length es
     vars <- getFresh $ length es
     [once] <- getFresh 1
     es' <- zipWithM desExp chans es
     let joins = zipWith (\chan var -> VarJ chan [VarP var]) chans vars
     return $ DefA [ReactionD ((VarJ once []):joins) 0
                              (Proc [MsgA k [ConE n $ map VarE vars]])]
                   (Proc $ (MsgA once []):es')

-- | A call without any arguments is simply a message with the current
-- continuation.
desExp k (CallS v []) = return $ MsgA v [VarE k]

-- | A call with values requires us to compute the values first (in parallel),
-- and then sending the results and the current continuation to the called
-- function.
desExp k (CallS v es) =
  do chans <- getFresh $ length es
     vars <- getFresh $ length es
     [once] <- getFresh 1
     let joins = zipWith (\chan var -> VarJ chan [VarP var]) chans vars
     es' <- zipWithM desExp chans es
     return $ DefA [ReactionD ((VarJ once []):joins) 0
                              (Proc [MsgA v (map VarE vars ++ [VarE k])])]
                   (Proc $ (MsgA once []):es')

-- |Desugaring of instruction lists. The continuation is used when desugaring
--  match expressions: When the nested instruction list in a branch is finished,
--  we will continue executing the statements right after the match expression.
desInstr :: Maybe Continuation -> [Instr] -> DesugarM [Atom]

-- | When we are done, we may need to send a signal on the continuation (see
-- desugaring of MatchI).
desInstr mk [] = maybe (return [InertA])
                       (\k -> return [MsgA k []]) mk

-- | We evaluate all expressions in a let binding in parallel, resuming
-- execution when they are all done.
desInstr mk ((LetI pats e):is) =
  do is' <- desInstr mk is
     [k, once] <- getFresh 2
     e' <- desExp k e
     return $ [DefA [ReactionD [VarJ once [], VarJ k pats] 0 (Proc is')]
                    (Proc [MsgA once [], e'])]

desInstr mk ((RunI (Proc as)):is) =  -- (as ++) <$> desInstr mk is
  (liftM2 (++)) (concat <$> mapM desAtom as) (desInstr mk is)

-- |A do statement is executed by evaluating the given call expression, and
-- throwing away the result.
desInstr mk ((DoI f es):is) =
  do is' <- desInstr mk is
     [k, once] <- getFresh 2
     f' <- desExp k (CallS f es)
     return $ [DefA [ReactionD [VarJ once [], VarJ k []] 0 (Proc is')]
                    (Proc [MsgA once [], f'])]

-- |When executing a match, we first evaluate the expression to be matched.
-- Each branch is given the same continuation so they can signal when they are
-- done executing.
desInstr mk ((MatchI e mps):is) =
  do is' <- desInstr mk is
     [l, m, var_e, once] <- getFresh 4
     e' <- desExp l e
     -- mk' is the continuation we give to each branch. If there are no
     -- instructions after this match instruction, we pass the current
     -- continuation. Otherwise, we pass a synchronisation continuation, m,
     -- that allows us to wait for the branch to finish, after which we resume
     -- execution.
     let mk' = if (is == []) then mk else Just m
     mps' <- mapM (\(pat, is) -> (,) pat <$> (Proc <$> desInstr mk' is)) mps
     return [DefA [ReactionD [VarJ once [], VarJ l [VarP var_e]] 0
                             (Proc $ [MatchA (VarE var_e) mps'])
                  ,ReactionD [VarJ m []] 0
                             (Proc $ is')]
                  (Proc [MsgA once [], e'])]

-- |When we return unit, simply send an empty message on the return-to
-- continuation in parallel with executing the rest of the instructions.
desInstr mk ((ReturnI [] f):is) =
  do is' <- desInstr mk is
     return $ (MsgA (contName f) []):is'

-- |Special case for tail call optimization. When the continuation is Nothing,
-- and the last statement is a synchronous call, we can safely evaluate the
-- call and pass the continuation of the return-to function.
desInstr Nothing [ReturnI [CallS f' es'] f] =
  do c' <- desExp (contName f) (CallS f' es')
     return  [c']

-- |When we return one or more values, we need to evaluate them first. We
-- evaluate the values in parallel, and then sends a message with the results
-- on the return-to continuation.
desInstr mk ((ReturnI es f):is) =
  do is' <- desInstr mk is
     chans <- getFresh $ length es
     vars <- getFresh $ length es
     [once] <- getFresh 1
     es' <- zipWithM desExp chans es
     let joins = zipWith (\chan var -> VarJ chan [VarP var]) chans vars
     return [DefA [ReactionD ((VarJ once []):joins) 0
                             (Proc $ (MsgA (contName f) $ map VarE vars):is')]
                  (Proc $ (MsgA once []):es')]

desAtom :: Atom -> DesugarM [Atom]
desAtom (DefA defs (Proc as)) =
  do defs' <- mapM desDef defs
     as' <- concat <$> mapM desAtom as
     return [DefA defs' (Proc as')]
desAtom (MatchA e mps) = do
    mps' <- mapM (\(pat, (Proc as)) -> (,) pat <$> (Proc . concat <$> mapM desAtom as)) mps
    return [MatchA e mps']
desAtom (InstrA is) =
   do is' <- desInstr Nothing is
      return is'
desAtom (DelayA d (Proc as)) = do
  as' <- concat <$> mapM desAtom as
  return $ [DelayA d (Proc as')]
desAtom a = return [a]

desDef :: Def -> DesugarM Def
desDef (ReactionD js d (Proc as)) =
  ReactionD (map desJoin js) d <$> (Proc . concat <$> mapM desAtom as)
desDef (LocationD name ds (Proc as)) = do
  ds' <- mapM desDef ds
  as' <- concat <$> mapM desAtom as
  return $ LocationD name ds' (Proc as')

desJoin :: Join -> Join
desJoin (SyncJ f ps) = VarJ f (ps ++ [VarP $ contName f])
desJoin j = j
