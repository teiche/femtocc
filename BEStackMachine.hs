--{-# LANGUAGE InstanceSigs #-}

module BEStackMachine where

import Data.List (intercalate)
import Data.Maybe (fromJust)    
import Numeric
    
import Syntax
import FrameOffsetTable
    
-- A backend using a stack machine execution model


data Operand = Reg Int
             | Imm Int
             | Label String
             | Plus Operand Operand

instance Show Operand where
    show (Reg r) = "r" ++ (show r)
    show (Imm i) = show i
    show (Label l) = l
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
                 | LabelDef String
                 | Store Operand Operand
                 | Rsp Operand
                 | Call Operand
                 | Ret
                 | Nop
                 deriving Show

-- Determine how much a single instruction changes the stack size
stackChange :: Instruction -> Int
stackChange Push{} = 1
stackChange Pop{} = -1
stackChange Call{} = 1
stackChange Ret{} = -1
stackChange (Asp (Imm x)) = negate x                    
stackChange _ = 0
                            
-- Determine how much the stack grows or shrinks after executing a
-- list of instructions
-- Positive values are the stack growing
-- Negative values are the satck shrinking
stackDelta :: [Instruction] -> Int
stackDelta = sum . (map stackChange)

-- Push the accumulator onto the stack, making room for an expression to be evaluated
pushDown = [Push acc]

binOpFunc :: String -> Operand -> Operand -> Instruction           
binOpFunc "+" = Add
binOpFunc "-" = Sub
binOpFunc x = error $ "Uncrecognized Operator: " ++ x                

-- The minimum number of stack words a sequence of expressions will require to execute
stackSize :: [Expr] -> Int
stackSize exprs = sum $ map exprSize exprs
    
exprSize :: Expr -> Int
exprSize VariableDecl{} = 1
exprSize _ = 0
                  
funcEntry :: Expr -> [Instruction]
funcEntry (FuncDef _ _ _ body) =
    [Push cfp, -- Save the current frame pointer
     Rsp cfp, -- Calculate the new frame pointer
     Asp ((Imm . negate . stackSize) body)] -- Make stack room for the function

funcExit :: Expr -> [Instruction]
funcExit (FuncDef _ _ _ body) =
    [Asp ((Imm . stackSize) body),
     Pop cfp,
     Ret]

-- Get the stack size required to pass all function arguments    
argStackSize :: [Symbol] -> Int
argStackSize args = length args

codeGen :: Expr -> [Instruction]
codeGen = codeGen' []

codeGen' :: FrameOffsetTable -> Expr -> [Instruction]
codeGen' _   (Const v) = [Mov acc (Imm v)]
codeGen' fot (BinOp "=" (Identifier dest) src) =
    (codeGen' fot src) ++
    [Store acc (cfp `Plus` ((Imm . fromJust) $ lookup dest fot))]
codeGen' fot (BinOp "=" dest src) =
    (codeGen' fot dest) ++
    pushDown ++
    (codeGen' fot src) ++
    [Pop tmp, Store acc tmp]
codeGen' fot (BinOp op left right) =
    (codeGen' fot left) ++
    pushDown ++
    (codeGen' fot right) ++
    [Pop tmp, binOpFunc op acc tmp]
codeGen' _   (VariableDecl _) = []
codeGen' [] f@(FuncDef name _ args body) =
    [LabelDef name] ++
     funcEntry f ++
     (concatMap (codeGen' (generateFunctionOffset f)) body) ++
     funcExit f++
    [LabelDef ("DBG_" ++ name ++ "_end")]
codeGen' fot f@(FuncCall name args) =
    pushArgs ++
    -- Go to the function
    [Call (Label name),
    -- Clean up the stack after returning
     Asp ((Imm . stackDelta) pushArgs)]
                      -- Evaluate all args, and push them onto the stack
    where pushArgs = ((intercalate pushDown) $ map (codeGen' fot) args) ++ pushDown
          
codeGen' _ Pass = [Nop]
codeGen' _ _ = []    

emitASM :: Instruction -> Either String String
emitASM (Push r@Reg{}) = Right $ "push " ++ (show r)
emitASM (Pop r@Reg{})  = Right $ "pop " ++ (show r)
emitASM (Add rd@Reg{} rs@Reg{}) = Right $ "add " ++ (show rd) ++ ", " ++ (show rs)
emitASM (Sub rd@Reg{} rs@Reg{}) = Right $ "sub " ++ (show rd) ++ ", " ++ (show rs)
emitASM (Mov rd@Reg{} i@Imm{})  = Right $ "mov " ++ (show rd) ++ ", " ++ (show i)
emitASM (Asp i@Imm{})           = Right $ "asp " ++ (show i)
emitASM (Store src@Reg{} dest@Reg{}) = Right $ "st " ++ (show src) ++ ", " ++ (show dest)
emitASM (Store src@Reg{} dest@(Plus Reg{} Imm{})) = Right $ "st " ++ (show src) ++ ", " ++ (show dest)
emitASM (LabelDef lbl)             = Right $ "lbl" ++ ":"
emitASM (Rsp r@Reg{})           = Right $ "rsp " ++ (show r)
emitASM (Call l@Label{})          = Right $ "call " ++ show l
emitASM Ret                     = Right "ret"
emitASM Nop                     = Right "nop"
emitASM instr = Left $ "Undefined Instruction: " ++ (show instr)

compileAST = (fmap emitASM) . codeGen
