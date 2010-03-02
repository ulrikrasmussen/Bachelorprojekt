module Language(Expr(..), Pat(..), Join(..), Def(..), Proc(..)) where

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
          | AndJ Join Join
    deriving (Eq)

instance Show Join where
    show (VarJ v ps) =
     v ++ "(" ++ (concat $ intersperse ", " (map show ps)) ++ ")"

data Def  = EmptyD
          | ReactionD Join Proc
          | OrD Def Def
    deriving (Eq)

instance Show Def where
  show EmptyD = "T"
  show (ReactionD j p) = show j ++ " |> " ++ show p
  show (OrD d1 d2) = show d1 ++ " or " ++ show d2

data Proc = InertP
          | MsgP String [Expr]
          | AndP Proc Proc
          | DefP Def Proc
          | MatchP Expr [(Pat, Proc)]
    deriving (Eq)

instance Show Proc where
  show InertP = "0"
  show (MsgP s es) =
    s ++ "(" ++ (concat $ intersperse ", " (map show es)) ++ ")"
  show (AndP p1 p2) = show p1 ++ " & " ++ show p2
  show (DefP d p) = "def " ++ show d ++ " in " ++ show p
  show (MatchP e mps) = "match " ++ show e ++ " with " ++
    (concat $ intersperse " | " (map showmp mps))
    where showmp (pat, proc) = show pat ++ " -> " ++ show proc
