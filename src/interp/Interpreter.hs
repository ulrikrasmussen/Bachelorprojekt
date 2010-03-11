{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Interpreter(runInterpreter, Context) where

import Language

import Control.Monad
import Control.Applicative
import Control.Monad.State

import Data.List
import qualified Data.Map as M

--instance Applicative (State Context) where
--    pure = return
--    (<*>) = ap

newtype JoinM a = J { runJoinM :: State Context a  }
    deriving (Monad, MonadState Context, Functor)

data Context = Context { cDefs :: [Def]
                       , cAtoms :: [Atom]
                       , cFreshNames :: [String] }
 deriving (Eq)

instance Show Context where
  show context = "Defs:\n\t" ++ (concat . intersperse "\n\t" $ map show (cDefs context))
               ++ "\n\nAtoms:\n\t" ++ (concat . intersperse "\n\t" $ map show (cAtoms context))

class (Monad m) => MonadJoin m where
  getC :: m Context
  putC :: Context -> m ()
  getFreshNames :: Int -> m [String]

instance MonadJoin JoinM where
  getC = get
  putC = put
  getFreshNames n = do
    (fns, fns') <- gets (splitAt n . cFreshNames)
    modify $ \s -> s {cFreshNames = fns'}
    return fns

initContext :: [Def] -> [Atom] -> Context
initContext ds as = Context ds as ['#' : show i | i <- [1..]]

runInterpreter :: Proc -> Context
runInterpreter (Proc atms) = execState (runJoinM interp) (initContext [] atms)

interp :: (MonadJoin m, Functor m) => m ()
interp = do
  (Context ds as _) <- getC
  heatAtoms
  (Context ds' as' _) <- getC
  when (ds /= ds' && as /= as') interp

heatAtoms :: (MonadJoin m, Functor m) => m ()
heatAtoms = do
    atoms <- cAtoms <$> getC
    (ds', as') <- foldl1 (\(a,b) (c,d) -> (a++c, b++d)) <$> mapM heatAtom atoms
    context <- getC
    putC $ context { cDefs = cDefs context ++ ds', cAtoms = as' }
    return ()

heatAtom :: (MonadJoin m, Functor m) => Atom -> m ([Def], [Atom])
heatAtom InertA        = return ([],[])
heatAtom m@(MsgP _ _)  = return ([],[m])
heatAtom d@(DefA ds p)   = do
    let dvs = concatMap definedVars ds
    dvs' <- concatMap definedVars . cDefs <$> getC
    let ivs = dvs `intersect` dvs'
    sigma <- M.fromList . zipWith (\l r -> (l, l ++ r)) ivs <$> (getFreshNames $ length ivs)
    return (subst sigma <$> ds, pAtoms $ sigma `subst` p)
