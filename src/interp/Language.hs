module Language(Expr(..), Pat(..), Join(..), Def(..), Atom(..), Proc(..)) where

import Data.List (intersperse)

data Expr = VarE String
          | ZeroE
          | SuccE Expr
    deriving (Eq)

instance Show Expr where
  show (VarE v) = v
  show ZeroE = "Z"
  show (SuccE e) = "S " ++ show e

data Pat  = VarP String
          | ZeroP
          | SuccP Pat
    deriving (Eq)

instance Show Pat where
  show (VarP v) = v
  show ZeroP = "Z"
  show (SuccP e) = "S " ++ show e

data Join = VarJ String [Pat]
    deriving (Eq)

instance Show Join where
    show (VarJ v ps) =
     v ++ "(" ++ (concat $ intersperse ", " (map show ps)) ++ ")"

data Def  = ReactionD [Join] Proc
    deriving (Eq)

instance Show Def where
  show (ReactionD j p) = (concat $ intersperse " & " (map show j)) ++ " |> " ++ show p

newtype Proc = Proc [Atom]
   deriving (Eq)

instance Show Proc where
    show (Proc as) = concat $ intersperse " & " (map show as)

data Atom = InertA
          | MsgP String [Expr]
          | DefA [Def] Proc
          | MatchA Expr [(Pat, Proc)]
    deriving (Eq)

instance Show Atom where
  show InertA = "0"
  show (MsgP s es) =
    s ++ "(" ++ (concat $ intersperse ", " (map show es)) ++ ")"
  show (DefA d p) = "def " ++ (concat $ intersperse " or " (map show d)) ++ " in " ++ show p
  show (MatchA e mps) = "(match " ++ show e ++ " with " ++
    (concat $ intersperse " | " (map showmp mps)) ++ ")"
    where showmp (pat, proc) = show pat ++ " -> " ++ show proc
