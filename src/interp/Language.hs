{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Language( Expr(..)
               , SExpr(..)
               , Pat(..)
               , Join(..)
               , Def(..)
               , Atom(..)
               , Instr(..)
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
import qualified Data.Set as S

type Sigma = M.Map String Expr

class Subst a where subst :: Sigma -> a -> a

--instance Subst [Char] where
--  subst sigma v = maybe v id $ M.lookup v sigma

data Expr = VarE String
          | ZeroE
          | SuccE Expr
    deriving (Eq, Data, Typeable)

data SExpr = VarS String
           | ZeroS
           | SuccS SExpr
           | CallS String [SExpr]
    deriving (Eq, Data, Typeable)

instance Show SExpr where
    show (VarS v) = v
    show ZeroS = "0"
    show (SuccS e) = show' 1 e
      where show' n (SuccS e) = show' (n+1) e
            show' n ZeroS = show n
            show' n e = show e ++ " + " ++ show n
    show (CallS v es) = v ++ "(" ++ (concat . intersperse "," . map show $ es) ++ ")"

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
          | SyncJ String [Pat]
    deriving (Eq, Data, Typeable)

instance Show Join where
    show (VarJ v ps) =
     v ++ "<" ++ (concat $ intersperse ", " (map show ps)) ++ ">"
    show (SyncJ v ps) =
     v ++ "(" ++ (concat . intersperse ", " . map show $ ps) ++ ")"

instance Subst Join where
  subst sigma vj@(VarJ v ps) = 
   maybe vj (\(VarE v') -> VarJ v' ps) $ M.lookup v sigma


data Def  = ReactionD [Join] Proc
          | LocationD String [Def] Proc
    deriving (Eq, Data, Typeable)

instance Show Def where
  show (ReactionD j p) = (concat $ intersperse " & " (map show j)) ++ " |> " ++ show p

instance Subst Def where
  subst sigma (ReactionD js p) =
     let sigma' = foldl (flip M.delete) sigma (S.toList . S.unions $ map receivedVars js)
     in ReactionD (subst sigma' <$> js) (sigma' `subst` p)


newtype Proc = Proc {pAtoms :: [Atom]}
   deriving (Eq, Data, Typeable)

instance Show Proc where
    show (Proc as) = concat $ intersperse " & " (map show as)

instance Subst Proc where
  subst sigma (Proc as) = Proc (subst sigma <$> as)


data Atom = InertA
          | MsgA   String [Expr]
          | DefA   [Def] Proc
          | MatchA Expr [(Pat, Proc)]
          | InstrA [Instr]
    deriving (Eq, Data, Typeable)

instance Show Atom where
  show InertA = "0"
  show (MsgA s es) =
    s ++ "<" ++ (concat $ intersperse ", " (map show es)) ++ ">"
  show (DefA d p) = "def " ++ (concat $ intersperse " or " (map show d)) ++ " in " ++ show p
  show (MatchA e mps) = "(match " ++ show e ++ " with " ++
    (concat $ intersperse " | " (map showmp mps)) ++ ")"
    where showmp (pat, proc) = show pat ++ " -> " ++ show proc
  show (InstrA is) = "{" ++ (concat . intersperse "; " . map show $ is) ++ "}"

instance Subst Atom where
  subst sigma (MsgA s es) = 
    let es' = subst sigma <$> es
    in  maybe (MsgA s es')
              (\(VarE s') -> MsgA s' es') $ M.lookup s sigma
  subst _ InertA = InertA
  subst sigma (DefA ds p) =
    let sigma' = foldl (flip M.delete) sigma (S.toList . S.unions $ map definedVars ds)
    in DefA (subst sigma' <$> ds) (sigma' `subst` p)
  subst sigma (MatchA e mps) = MatchA (sigma `subst` e) (substPat <$> mps)
    where substPat (pat, proc) =
            let sigma' = foldl (flip M.delete) sigma (S.toList $ receivedVars pat)
            in  (pat, sigma' `subst` proc)

data Instr = LetI Pat SExpr
           | RunI Proc
           | DoI String [SExpr]
           | MatchI SExpr [(Pat, [Instr])]
           | ReturnI [SExpr] String
    deriving (Eq, Data, Typeable)

instance Show Instr where
    show (LetI p e) = "let " ++ show p ++ " = " ++ show e
    show (RunI p) = "run " ++ show p
    show (DoI v es) = "do " ++ v ++ "(" ++ (concat . intersperse "," $ map show es) ++ ")"
    show (MatchI e mps) = "match " ++ show e ++ " with " ++ 
        (concat $ intersperse " | " (map showmp mps))
        where showmp (pat, is) = show pat ++ " -> " ++ "{" ++ showis is ++ "}"
              showis = concat . intersperse ";\n" . map show
    show (ReturnI es v) = "return (" ++ (concat . intersperse "," $ map show es)
                                     ++ ") to " ++ v

-- Note: This must not contain duplicates (because the vars are received in the same scope)
class ReceivedVars e where receivedVars :: e -> S.Set String

instance ReceivedVars Pat where
  receivedVars ZeroP = S.empty
  receivedVars (SuccP p) = receivedVars p
  receivedVars (VarP v) = S.singleton v

instance ReceivedVars Join where
  receivedVars (VarJ m ps) = S.unions $ map receivedVars ps

class DefinedVars e where definedVars :: e -> S.Set String

instance DefinedVars Join where
  definedVars (VarJ m ps) = S.singleton m

instance DefinedVars Def where
  definedVars (ReactionD j p) = S.unions $ map definedVars j

class FreeVars e where freeVars :: e -> S.Set String

instance FreeVars Expr where
  freeVars (ZeroE)   = S.empty
  freeVars (SuccE e) = freeVars e
  freeVars (VarE v)  = S.singleton v

instance FreeVars Def where
  freeVars (ReactionD j p) =
    S.unions (map definedVars j)
        `S.union` (freeVars p `S.difference` S.unions (map receivedVars j))

instance FreeVars Proc where
  freeVars (Proc as) = S.unions $ map freeVars as

instance FreeVars Atom where
  freeVars (InertA) = S.empty
  freeVars (MsgA m es) = m `S.insert` S.unions (map freeVars es)
  freeVars (DefA ds p) = (freeVars p `S.union` S.unions (map freeVars ds))
    `S.difference` S.unions (map definedVars ds)
  freeVars (MatchA e mps) = freeVars e `S.union` (S.unions $ map freeVars' mps)
    where freeVars' (pat, proc) = freeVars proc `S.union` receivedVars pat

class LiveVars a where liveVars :: a -> S.Set String

instance LiveVars Proc where 
  liveVars p = S.unions $ map liveVars $ pAtoms p

instance LiveVars Atom where 
  liveVars (InertA) = S.empty
  liveVars m@(MsgA _ _) = freeVars m
  liveVars (DefA ds p) = (S.unions $ map liveVars ds) `S.union` ((liveVars p) `S.difference` (S.unions $ map definedVars ds ))
  liveVars (MatchA e mps) = (S.unions $ map liveVars' mps) `S.difference` freeVars e
    where liveVars' (pat, proc) = liveVars proc `S.difference` receivedVars pat

instance LiveVars Def where 
  liveVars d@(ReactionD js p) = (liveVars p) `S.difference` ((S.unions $ map definedVars js) `S.union` (S.unions $ map receivedVars js))
