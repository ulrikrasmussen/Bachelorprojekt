{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Language( Expr(..)
               , Pat(..)
               , Join(..)
               , Def(..)
               , Atom(..)
               , Proc(..)
               , definedVars
               , freeVars
               , receivedVars
               , liveVars
               , subst
               , Sigma
                 ) where

import Control.Arrow
import Control.Applicative
import Data.List (intersperse, (\\), nub, union)
import Data.Generics
import qualified Data.Map as M

type Sigma = M.Map String Expr

class Subst a where subst :: Sigma -> a -> a

--instance Subst [Char] where
--  subst sigma v = maybe v id $ M.lookup v sigma

data Expr = VarE String
          | ZeroE
          | SuccE Expr
    deriving (Eq, Data, Typeable)

--instance Show Expr where
--  show (VarE v) = v
--  show ZeroE = "Z"
--  show (SuccE e) = "S " ++ show e

instance Show Expr where
    show (VarE v) = v
    show ZeroE = "0"
    show succExpr = show' 0 succExpr
      where show' n (SuccE e)= show' (n+1) e
            show' n ZeroE = show n
            show' n (VarE v) = v ++ " + " ++ show n

instance Subst Expr where
    subst sigma expr =
      case expr of
        ve@(VarE v) -> maybe ve id $ M.lookup v sigma
        ZeroE -> ZeroE
        SuccE e -> SuccE $ sigma `subst` e


data Pat  = VarP String
          | ZeroP
          | SuccP Pat
    deriving (Eq, Data, Typeable)

instance Show Pat where
    show (VarP v) = v
    show ZeroP = "0"
    show succPat = show' 0 succPat
      where show' n (SuccP e)= show' (n+1) e
            show' n ZeroP = show n
            show' n (VarP v) = v ++ " + " ++ show n

--instance Subst Pat where
--  subst sigma (VarP v) = VarP $ sigma `subst` v
--  subst sigma ZeroP = ZeroP
--  subst sigma (SuccP e) = SuccP $ sigma `subst` e


{- A join pattern -}
data Join = VarJ String [Pat]
    deriving (Eq, Data, Typeable)

instance Show Join where
    show (VarJ v ps) =
     v ++ "(" ++ (concat $ intersperse ", " (map show ps)) ++ ")"

instance Subst Join where
  subst sigma vj@(VarJ v ps) = 
   maybe vj (\(VarE v') -> VarJ v' ps) $ M.lookup v sigma


data Def  = ReactionD [Join] Proc
    deriving (Eq, Data, Typeable)

instance Show Def where
  show (ReactionD j p) = (concat $ intersperse " & " (map show j)) ++ " |> " ++ show p

instance Subst Def where
  subst sigma (ReactionD js p) =
     let sigma' = foldl (flip M.delete) sigma (concatMap receivedVars js)
     in ReactionD (subst sigma' <$> js) (sigma' `subst` p)


newtype Proc = Proc {pAtoms :: [Atom]}
   deriving (Eq, Data, Typeable)

instance Show Proc where
    show (Proc as) = concat $ intersperse " & " (map show as)

instance Subst Proc where
  subst sigma (Proc as) = Proc (subst sigma <$> as)


data Atom = InertA
          | MsgA String [Expr]
          | DefA [Def] Proc
          | MatchA Expr [(Pat, Proc)]
    deriving (Eq, Data, Typeable)

instance Show Atom where
  show InertA = "0"
  show (MsgA s es) =
    s ++ "(" ++ (concat $ intersperse ", " (map show es)) ++ ")"
  show (DefA d p) = "def " ++ (concat $ intersperse " or " (map show d)) ++ " in " ++ show p
  show (MatchA e mps) = "(match " ++ show e ++ " with " ++
    (concat $ intersperse " | " (map showmp mps)) ++ ")"
    where showmp (pat, proc) = show pat ++ " -> " ++ show proc

instance Subst Atom where
  subst sigma (MsgA s es) = 
    let es' = subst sigma <$> es
    in  maybe (MsgA s es')
              (\(VarE s') -> MsgA s' es') $ M.lookup s sigma
  subst _ InertA = InertA
  subst sigma (DefA ds p) =
    let sigma' = foldl (flip M.delete) sigma (concatMap definedVars ds)
    in DefA (subst sigma' <$> ds) (sigma' `subst` p)
  subst sigma (MatchA e mps) = MatchA (sigma `subst` e) (substPat <$> mps)
    where substPat (pat, proc) =
            let sigma' = foldl (flip M.delete) sigma (receivedVars pat)
            in  (pat, sigma' `subst` proc)


-- Note: This must not contain duplicates (because the vars are received in the same scope)
class ReceivedVars e where receivedVars :: e -> [String]

instance ReceivedVars Pat where
  receivedVars ZeroP = []
  receivedVars (SuccP p) = receivedVars p
  receivedVars (VarP v) = [v]

instance ReceivedVars Join where
  receivedVars (VarJ m ps) = concatMap receivedVars ps

class DefinedVars e where definedVars :: e -> [String]

instance DefinedVars Join where
  definedVars (VarJ m ps) = [m]

instance DefinedVars Def where
  definedVars (ReactionD j p) = concatMap definedVars j

class FreeVars e where freeVars :: e -> [String]

instance FreeVars Expr where
  freeVars (ZeroE)   = []
  freeVars (SuccE e) = freeVars e
  freeVars (VarE v)  = [v]

instance FreeVars Def where
  freeVars (ReactionD j p) = concatMap definedVars j ++ (freeVars p \\ concatMap receivedVars j)

instance FreeVars Proc where
  freeVars (Proc as) = concatMap freeVars as

instance FreeVars Atom where
  freeVars (InertA) = []
  freeVars (MsgA m es) = m : concatMap freeVars es
  freeVars (DefA ds p) = (freeVars p ++ concatMap freeVars ds) \\ concatMap definedVars ds
  freeVars (MatchA e mps) = freeVars e ++ (concatMap freeVars' mps)
    where freeVars' (pat, proc) = freeVars proc \\ receivedVars pat

class LiveVars a where liveVars :: a -> [String]

instance LiveVars Proc where 
  liveVars p = nub $ concatMap liveVars $ pAtoms p

instance LiveVars Atom where 
  liveVars (InertA) = []
  liveVars m@(MsgA _ _) = freeVars m
  liveVars (DefA ds p) = (nub $ concatMap liveVars ds) `union` ((liveVars p) \\ (nub $concatMap definedVars ds ))
  liveVars (MatchA e mps) = (foldl union [] $ map liveVars' mps) \\ freeVars e
    where liveVars' (pat, proc) = liveVars proc \\ receivedVars pat

instance LiveVars Def where 
  liveVars d@(ReactionD js p) = (liveVars p) \\ ((nub $ concatMap definedVars js) `union` (nub $ concatMap receivedVars js))
