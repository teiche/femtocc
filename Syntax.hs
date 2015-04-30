module Syntax where

import Data.Function
    
type Name = String
type Indirection = Int

data Symbol = Symbol Name Type
     deriving (Eq, Show)

symbolName :: Symbol -> String
symbolName (Symbol name _) = name              

-- Because C is generally type-unsafe, two symbols are equal if their names are equal
--instance Eq Symbol where
--    (==) = (==) `on` symbolName

data Type = Type PrimType Indirection
     deriving (Eq, Show)

data PrimType = Int
              | Function Type [Type]
     deriving (Eq, Show)

data Expr
     = BinOp Name Expr Expr
     | FuncDef Name Type [Symbol] [Expr]
     | FuncCall Name [Expr]
     | VariableDecl Symbol
     | If Expr [Expr]
     | Const Integer
     | Assignment Expr Expr
     | Identifier Name
     | Return (Maybe Expr)
     | Nop
     deriving (Eq, Show)
