module FrameOffsetTable where

import Syntax
    
type Offset = Int
    
type Scope = [(SymbolName, Offset)]
type FrameOffsetTable = Scope

-- Stack Layout for Function Call:

-- LOW ADDRESSES

--  Scope Variable n
--  Scope Variable 1
--  Scope Variable 0
-- Last Frame Pointer        <-  Current Frame Pointer
--   Return Address
--       Arg n
--       Arg 1
--       Arg 0
--        ...
---Bottom of Stack---
-- HIGH ADDRESSES

-- Distance in stack words from the frame pointer to the last argument
distFPToLastArg = 2
    
generateScope :: Offset -> [Expr] -> Scope
generateScope i (e:es) = newSymbols ++ (generateScope (i - offset) es)
  where calcOffset (VariableDecl (Symbol symName _)) = [(symName, i - 1)]
        calcOffset _ = []
        newSymbols = calcOffset e
        offset = length newSymbols
generateScope i [] = []
                     
generateFunctionOffset :: Expr -> Scope
generateFunctionOffset (FuncDef _ _ vars body) = (generateScope 0 body) ++ (generateScope argOffset varDecls)
  where varDecls = map VariableDecl vars
        -- Offset of first argument.  Arguments grow toward low addresses
        argOffset = (length vars) + distFPToLastArg
                     
