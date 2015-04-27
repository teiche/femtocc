module Syntax where

type Name = String
type Indirection = Int

data Variable = Variable Name Type
     deriving (Eq, Show)

data Type = Type PrimType Indirection
     deriving (Eq, Show)

data PrimType = Int
              | Function Type [Type]
     deriving (Eq, Show)

data Expr
     = BinOp Name Expr Expr
     | FuncDef Name Type [Variable] [Expr]
     | FuncCall Name [Expr]
     | If Expr [Expr]
     | Const Integer
     | Assignment Expr Expr
     | Identifier Name
     | Return (Maybe Expr)
     | Nop
     deriving (Eq, Show)
