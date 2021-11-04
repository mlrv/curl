module Model where

type Name = String

data Expr
  = ATOM Atom
  | LIST [Expr]
  deriving (Eq, Read, Show)

data Atom
  = Int Int
  | Symbol Name
  deriving (Eq, Read, Show)