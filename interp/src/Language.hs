module Language(Expr(..), Pat(..), Join(..), Def(..), Proc(..)) where

data Expr = VarE String
          | ZeroE
          | SuccE Expr

data Pat  = VarP String
          | ZeroP
          | SuccP Pat

data Join = VarJ String [Pat]
          | AndJ Join Join

data Def  = EmptyD
          | ReactionD Join Proc
          | OrD Def Def

data Proc = InertP
          | MsgP String [Expr]
          | AndP Proc Proc
          | DefP Def Proc
          | MatchP [Expr] [([Pat], Proc)]
