-- vim:set foldmethod=marker foldmarker=--{,--}:
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Language( Expr(..)
               , SExpr(..)
               , Pat(..)
               , Join(..)
               , Def(..)
               , isLocationD
               , isReactionD
               , Atom(..)
               , Instr(..)
               , Proc(..)
               , definedVars
               , freeVars
               , receivedVars
               , subst
               , Sigma
               , toJoin
               , fromJoin
                 ) where

import Control.Arrow
import Control.Applicative
import Data.List (intersperse, (\\), nub, union)
import Data.Generics
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace (trace)

type Sigma = M.Map String Expr

class Subst a where subst :: Sigma -> a -> a

data Expr = VarE String
          | IntE Int
          | ConE String [Expr]
    deriving (Eq, Ord, Data, Typeable)

--{ Instances
instance Show Expr where
    show (VarE v) = v
    show (IntE i) = show i
    show (ConE n es) = n ++ "(" ++ (concat . intersperse ", " $ map show es) ++ ")"

instance Subst Expr where
    subst sigma expr =
      case expr of
        ve@(VarE v) -> maybe ve id $ M.lookup v sigma
        IntE n -> expr
        ConE n es -> ConE n $ map (subst sigma) es
--}

data SExpr = VarS String
           | ConS String [SExpr]
           | IntS Int
           | CallS String [SExpr]
    deriving (Eq, Ord, Data, Typeable)

--{ Instances
instance Show SExpr where
    show (VarS v) = v
    show (IntS i) = show i
    show (CallS v es) = v ++ "(" ++ (concat . intersperse "," . map show $ es) ++ ")"
    show (ConS n es) = n ++ "(" ++ (concat . intersperse ", " $ map show es) ++ ")"
--}

data Pat  = VarP String
          | IntP Int
          | ConP String [Pat]
    deriving (Eq, Ord, Data, Typeable)

--{ Instances
instance Show Pat where
    show (VarP v) = v
    show (IntP i) = show i
    show (ConP n ps) = n ++ "(" ++ (concat . intersperse ", " $ map show ps) ++ ")"
--}

{- A join pattern -}
data Join = VarJ String [Pat]
          | SyncJ String [Pat]
    deriving (Eq, Ord, Data, Typeable)

--{ Instances
instance Show Join where
    show (VarJ v ps) =
     v ++ "<" ++ (concat $ intersperse ", " (map show ps)) ++ ">"
    show (SyncJ v ps) =
     v ++ "(" ++ (concat . intersperse ", " . map show $ ps) ++ ")"

instance Subst Join where
  subst sigma vj@(VarJ v ps) =
   maybe vj (\(VarE v') -> VarJ v' ps) $ M.lookup v sigma
--}


data Def  = ReactionD [Join] Integer Proc
          | LocationD String [Def] Proc
    deriving (Eq, Ord, Data, Typeable)

--{ Utility functions
isLocationD (LocationD _ _ _) = True
isLocationD _ = False
isReactionD (ReactionD _ _ _) = True
isReactionD _ = False
--}

--{ Instances
instance Show Def where
  show (ReactionD j d p) = (concat $ intersperse " & " (map show j))
    ++ " |>^" ++ show d ++ " " ++ show p
  show (LocationD n d p) =
    n ++ "[" ++ (concat $ intersperse " or " (map show d)) ++ " in " ++ (show p) ++ "]"

instance Subst Def where
  subst sigma (ReactionD js d p) =
     let sigma' = foldl (flip M.delete) sigma (S.toList . S.unions $ map receivedVars js)
     in ReactionD (subst sigma' <$> js) d (sigma' `subst` p)
  subst sigma (LocationD loc ds p) =
     let loc' = maybe loc (\(VarE loc') -> loc') $ M.lookup loc sigma
     in LocationD loc' (subst sigma <$> ds) (sigma `subst` p)
--}

newtype Proc = Proc {pAtoms :: [Atom]}
   deriving (Eq, Ord, Data, Typeable)

--{ Instances
instance Show Proc where
    show (Proc as) = concat $ intersperse " & " (map show as)

instance Subst Proc where
  subst sigma (Proc as) = Proc (subst sigma <$> as)
--}

data Atom = InertA
          | MsgA   String [Expr]
          | DefA   [Def] Proc
          | MatchA Expr [(Pat, Proc)]
          | InstrA [Instr]
          | DelayA Integer Proc
    deriving (Eq, Ord, Data, Typeable)

--{ Instances
instance Show Atom where
  show InertA = "0"
  show (MsgA s es) =
    s ++ "<" ++ (concat $ intersperse ", " (map show es)) ++ ">"
  show (DefA d p) = "def\n" ++ (indent $ concat $ intersperse " or " (map show d)) ++ "\n in\n" ++ (indent $ show p)
  show (MatchA e mps) = "(match " ++ show e ++ " with " ++
    (concat $ intersperse " | " (map showmp mps)) ++ ")"
    where showmp (pat, proc) = show pat ++ " -> " ++ show proc
  show (InstrA is) = "{ " ++ (concat . intersperse "\n; " . map show $ is) ++ "}"
  show (DelayA d p) = show d ++ " : " ++ show p

indent lins = indent' (lines lins)
  where
    indent' []     = []
    indent' (l:ls) = "    " ++ (l++"\n") ++ (indent' ls)

instance Subst Atom where
  subst sigma (MsgA s es) =
    let es' = subst sigma <$> es
    in  maybe (MsgA s es')
              --(\(VarE s') -> MsgA s' es') $ M.lookup s sigma
              {- debug: -}(\a -> case a of { VarE s' -> MsgA s' es'; _ -> error ("Unmatching subst: " ++ show a ++ "\nsigma: " ++ show sigma)}) $ M.lookup s sigma
  subst _ InertA = InertA
  subst sigma (DefA ds p) =
    let sigma' = foldl (flip M.delete) sigma (S.toList . S.unions $ map definedVars ds)
    in DefA (subst sigma' <$> ds) (sigma' `subst` p)
  subst sigma (MatchA e mps) = MatchA (sigma `subst` e) (substPat <$> mps)
    where substPat (pat, proc) =
            let sigma' = foldl (flip M.delete) sigma (S.toList $ receivedVars pat)
            in  (pat, sigma' `subst` proc)
  subst sigma (DelayA d p) = DelayA d $ sigma `subst` p
  subst sigma x = trace ("#subst# : \n\tx=" ++ show x ++ "\n\t sigma=" ++ show sigma) x

--}

data Instr = LetI [Pat] SExpr
           | RunI Proc
           | DoI String [SExpr]
           | MatchI SExpr [(Pat, [Instr])]
           | ReturnI [SExpr] String
    deriving (Eq, Ord, Data, Typeable)

--{ Instances
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
--}

-- Note: This must not contain duplicates (because the vars are received in the same scope)
class ReceivedVars e where receivedVars :: e -> S.Set String

--{ Instances
instance ReceivedVars Pat where
  receivedVars (VarP v) = S.singleton v
  receivedVars (IntP i) = S.empty
  receivedVars (ConP n ps) = S.unions . map receivedVars $ ps

instance ReceivedVars Join where
  receivedVars (VarJ m ps) = S.unions $ map receivedVars ps

--}

class DefinedVars e where definedVars :: e -> S.Set String

--{ Instances
instance DefinedVars Join where
  definedVars (VarJ m ps) = S.singleton m

instance DefinedVars Def where
  definedVars (ReactionD j d p) = S.unions $ map definedVars j
  definedVars (LocationD a ds p) = a `S.insert` S.unions (map definedVars ds)

--}

class FreeVars e where freeVars :: e -> S.Set String

--{ Instances
instance FreeVars Expr where
  freeVars (ConE n es) = S.unions $ map freeVars es
  freeVars (IntE _)    = S.empty
  freeVars (VarE v)    = S.singleton v

instance FreeVars Def where
  freeVars (ReactionD j d p) =
    S.unions (map definedVars j)
        `S.union` (freeVars p `S.difference` S.unions (map receivedVars j))
  freeVars (LocationD a ds p) =
    a `S.insert` S.unions (map freeVars ds)
      `S.union` freeVars p

instance FreeVars Proc where
  freeVars (Proc as) = S.unions $ map freeVars as

instance FreeVars Atom where
  freeVars (InertA) = S.empty
  freeVars (MsgA m es) = m `S.insert` S.unions (map freeVars es)
  freeVars (DefA ds p) = (freeVars p `S.union` S.unions (map freeVars ds))
    `S.difference` S.unions (map definedVars ds)
  freeVars (MatchA e mps) = freeVars e `S.union` (S.unions $ map freeVars' mps)
    where freeVars' (pat, proc) = freeVars proc `S.union` receivedVars pat
  freeVars (DelayA d p) = freeVars p
--}

--{ Conversion functions
class JoinValue a where
 fromJoin :: Expr -> a
 toJoin   :: a -> Expr

instance JoinValue Expr where
 fromJoin = id
 toJoin = id

instance JoinValue Bool where
 fromJoin (ConE "True" []) = True
 fromJoin (ConE "False" []) = False
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

instance (JoinValue a) => JoinValue (Maybe a) where
 fromJoin (ConE "Nothing" []) = Nothing
 fromJoin (ConE "Just" [x]) = Just $ fromJoin x

 toJoin Nothing = ConE "Nothing" []
 toJoin (Just x) = ConE "Just" [toJoin x]

instance JoinValue Char where
 fromJoin = toEnum . fromJoin
 toJoin = toJoin . fromEnum
--}
