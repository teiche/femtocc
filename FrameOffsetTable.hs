module FrameOffsetTable where

import Syntax
    
type Offset = Int
    
type Scope = [(SymbolName, Offset)]
type FrameOffsetTable = Scope
    
generateScope :: Offset -> [Expr] -> Scope
generateScope i (e:es) = newSymbols ++ (generateScope (i + offset) es)
  where calcOffset (VariableDecl (Symbol symName _)) = [(symName, i + 1)]
        calcOffset _ = []
        newSymbols = calcOffset e
        offset = length newSymbols
generateScope i [] = []
                     
generateFunctionOffset :: Expr -> Scope
generateFunctionOffset (FuncDef _ _ vars body) = generateScope 0 exprs
  where varDecls = map VariableDecl vars
        exprs = varDecls ++ body
                     
