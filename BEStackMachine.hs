--{-# LANGUAGE InstanceSigs #-}

module BEStackMachine where

import Data.Maybe (fromJust)    
import Numeric
    
import Syntax
import FrameOffsetTable
    
-- A backend using a stack machine execution model


data Operand = Reg Int
             | Imm Int
             | Plus Operand Operand

instance Show Operand where
    show (Reg r) = "r" ++ (show r)
    show (Imm i) = show i
    show (Plus r@Reg{} i@Imm{}) = "(" ++ (show r) ++ " + " ++ (show i) ++ ")"

ret = Reg 0 -- Return Value
cfp = Reg 1 -- Current frame pointer
acc = Reg 2 -- Accumulator
tmp = Reg 3 -- Temp

-- TODO: Rename to something less instruction-y
-- because this contains labels and directives as well

data Instruction = Push Operand
                 | Pop Operand
                 | Add Operand Operand
                 | Sub Operand Operand                   
                 | Mov Operand Operand
                 | Asp Operand
                 | Store Operand Operand
                 | Label String
                 | Nop
                 deriving Show

-- Push the accumulator onto the stack, making room for an expression to be evaluated
pushDown = [Push acc]

binOpFunc :: String -> Operand -> Operand -> Instruction           
binOpFunc "+" = Add
binOpFunc "-" = Sub
binOpFunc x = error $ "Uncrecognized Operator: " ++ x                

codeGen :: Expr -> [Instruction]
codeGen = codeGen' []
                
codeGen' :: FrameOffsetTable -> Expr -> [Instruction]
codeGen' _   (Const v) = [Mov acc (Imm v)]
codeGen' fot (BinOp "=" (Identifier dest) src) = (codeGen' fot src) ++
                                                  [Store acc (cfp `Plus` ((Imm . fromJust) $ lookup dest fot))]
codeGen' fot (BinOp "=" dest src) = (codeGen' fot dest) ++
                                     pushDown ++
                                     (codeGen' fot src) ++
                                     [Pop tmp, Store acc tmp]
codeGen' fot (BinOp op left right) = (codeGen' fot left) ++
                                pushDown ++
                                (codeGen' fot right) ++
                                [Pop tmp, binOpFunc op acc tmp]
codeGen' _   (VariableDecl _) = [Asp (Imm 1)] -- Make room for the variable
codeGen' [] f@(FuncDef name _ args body) =
    [Label name, Asp ((Imm . length) args)] ++ -- TODO: Size of arguments?
    (concatMap (codeGen' (generateFunctionOffset f)) body) ++
    [Label ("DBG_" ++ name ++ "_end")]
codeGen' _ _ = []    
    
                                     
codeGen' _ Pass = [Nop]

emitASM :: Instruction -> Either String String
emitASM (Push r@Reg{}) = Right $ "push " ++ (show r)
emitASM (Pop r@Reg{})  = Right $ "pop " ++ (show r)
emitASM (Add rd@Reg{} rs@Reg{}) = Right $ "add " ++ (show rd) ++ ", " ++ (show rs)
emitASM (Sub rd@Reg{} rs@Reg{}) = Right $ "sub " ++ (show rd) ++ ", " ++ (show rs)
emitASM (Mov rd@Reg{} i@Imm{})  = Right $ "mov " ++ (show rd) ++ ", " ++ (show i)
emitASM (Asp i@Imm{})           = Right $ "asp " ++ (show i)
emitASM (Store src@Reg{} dest@Reg{}) = Right $ "st " ++ (show src) ++ ", " ++ (show dest)
emitASM (Store src@Reg{} dest@(Plus Reg{} Imm{})) = Right $ "st " ++ (show src) ++ ", " ++ (show dest)
emitASM (Label lbl)             = Right $ lbl ++ ":"                                  
emitASM Nop                     = Right "nop"
emitASM instr = Left $ "Undefined Instruction: " ++ (show instr)

compileAST = (fmap emitASM) . codeGen
