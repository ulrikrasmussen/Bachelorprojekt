module Language(Expr(..), Pat(..), Join(..), Def(..), Proc(..)) where

data Expr = VarE String
          | ZeroE
          | SuccE Expr
    deriving (Show, Eq)

data Pat  = VarP String
          | ZeroP
          | SuccP Pat
    deriving (Show, Eq)

data Join = VarJ String [Pat]
          | AndJ Join Join
    deriving (Show, Eq)

data Def  = EmptyD
          | ReactionD Join Proc
          | OrD Def Def
    deriving (Show, Eq)

data Proc = InertP
          | MsgP String [Expr]
          | AndP Proc Proc
          | DefP Def Proc
          | MatchP [Expr] [([Pat], Proc)]
    deriving (Show, Eq)
