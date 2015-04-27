module Pretty where

import Syntax

prettyName :: Expr -> String
prettyName (BinOp name _ _) = name
prettyName (FuncDef name ty args _) = name
prettyName (FuncCall name args) = name ++ "()"
prettyName (If cond body) = "If" ++ (prettyName cond)
prettyName (Const int) = show int
prettyName (Assignment _ _) = "="
prettyName (Identifier name) = name
prettyName (Return _) = "Return"
prettyName (Nop) = "nop"
