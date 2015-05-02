--{-# LANGUAGE InstanceSigs #-}

module BEStackmachine where

import Numeric
    
import Syntax
    
-- A backend using a stack machine execution model

-- R0 is the accumulator
-- R1 is temporary storage for evaluating operations

data Operand = Reg Integer
             | Imm Integer

instance Show Operand where
    show (Reg r) = "r" ++ (show r)
    show (Imm i) = show i
                       
r0 = Reg 0
r1 = Reg 1
                       
data Instruction = Push Operand
                 | Pop Operand
                 | Add Operand Operand
                 | Sub Operand Operand                   
                 | Mov Operand Operand
                 | Nop
                 deriving Show

-- Push the accumulator onto the stack, making room for an expression to be evaluated
pushDown = [Push r0]

binOpFunc :: String -> Operand -> Operand -> Instruction           
binOpFunc "+" = Add
binOpFunc "-" = Sub
           
codeGen :: Expr -> [Instruction]
codeGen (Const v) = [Mov r0 (Imm v)]
codeGen (BinOp op left right) = (codeGen left) ++
                                pushDown ++
                                (codeGen right) ++
                                [Pop r1, binOpFunc op r0 r1]
codeGen Pass = [Nop]

emitASM :: Instruction -> Either String String
emitASM (Push r@Reg{}) = Right $ "push " ++ (show r)
emitASM (Pop r@Reg{})  = Right $ "pop " ++ (show r)
emitASM (Add rd@Reg{} rs@Reg{}) = Right $ "add " ++ (show rd) ++ ", " ++ (show rs)
emitASM (Sub rd@Reg{} rs@Reg{}) = Right $ "sub " ++ (show rd) ++ ", " ++ (show rs)
emitASM (Mov rd@Reg{} i@Imm{})  = Right $ "mov " ++ (show rd) ++ ", " ++ (show i)
emitASM Nop                     = Right "nop"
emitASM instr = Left $ "Undefined Instruction: " ++ (show instr)

compileAST = (fmap emitASM) . codeGen
