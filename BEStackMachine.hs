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

-- Comonly used registers
ret = Reg 0 -- Return Value
cfp = Reg 1 -- Current frame pointer
acc = Reg 2 -- Accumulator
tmp = Reg 3 -- Temp

data ASMStatement = Push Operand
                  | Pop Operand
                  | Add Operand Operand
                  | Sub Operand Operand
                  | Mov Operand Operand
                  | Asp Operand
                  | LabelDef String
                  | Store Operand Operand
                  | Rsp Operand
                  | Wsp Operand
                  | Call Operand
                  | Brn Operand
                  | Ret
                  | Nop
                  deriving Show

----------------------------------
-- Machine specific information --
----------------------------------
ramEnd :: Int
ramEnd = 0x8200

---------------
-- C Runtime --
---------------
-- Set up the stack and frame pointers
-- Jump to main
-- TODO: Handle this in the linking step
crtm :: [ASMStatement]
crtm = [
  -- pad out the interrupt vectors with NOPs
  Nop, Nop, Nop, Nop, Nop, Nop, Nop, Nop, Nop, Nop, Nop, Nop, Nop, Nop, Nop,
  -- Set up the stack pointer
  Mov tmp (Imm ramEnd),
  Wsp tmp,
  -- Initialize the frame pointer to main's frame
  Mov cfp (Imm 0),
  Brn (Label "main")
  ]

-- Determine how much a single instruction changes the stack size
stackChange :: ASMStatement -> Int
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
stackDelta :: [ASMStatement] -> Int
stackDelta = sum . (map stackChange)

-- The size a given expression requires on the stack ahead of time
exprSize :: Expr -> Int
exprSize VariableDecl{} = 1
exprSize _ = 0

-- The minimum number of stack words a sequence of expressions will require to execute
stackSize :: [Expr] -> Int
stackSize exprs = sum $ map exprSize exprs

-- Push the accumulator onto the stack, making room for an expression to be evaluated
pushDown = [Push acc]

binOpFunc :: String -> Operand -> Operand -> ASMStatement
binOpFunc "+" = Add
binOpFunc "-" = Sub
binOpFunc x = error $ "Uncrecognized Operator: " ++ x

-- Preallocate room on the stack for any locals declared in the function
funcAlloc :: Expr -> [ASMStatement]
funcAlloc (FuncDef _ _ _  body) = [Asp ((Imm . negate . stackSize) body)]

-- Adjust the frame pointer and make room for the new function
funcEntry :: Expr -> [ASMStatement]
funcEntry f =
    [Push cfp, -- Save the current frame pointer
     Rsp cfp] ++  -- Calculate the new frame pointer
     funcAlloc f

-- Deallocate room on the stack for locals declared in the scope
funcDealloc :: Expr -> [ASMStatement]
funcDealloc (FuncDef _ _ _ body) = [Asp ((Imm . stackSize) body)]

-- Deallocate room on the stack for locals, and restore the old frame pointer
funcExit :: Expr -> [ASMStatement]
funcExit f =
     funcDealloc f ++
    [Pop cfp,
     Ret]

-- Get the stack size required to pass all function arguments
-- TODO: Take into account argument size
argStackSize :: [Symbol] -> Int
argStackSize args = length args

---------------------
-- Code Generation --
---------------------

codeGen :: Expr -> [ASMStatement]
codeGen expr = crtm ++ (codeGen' [] expr)

codeGen' :: FrameOffsetTable -> Expr -> [ASMStatement]
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
-- Main is a special case, it doesn't have a frame pointer to restore
-- or return
codeGen' [] f@(FuncDef "main" _ args body) =
    [LabelDef "main"] ++
     funcAlloc f ++
     (concatMap (codeGen' (generateFunctionOffset f)) body) ++
     funcDealloc f++
    [LabelDef ("DBG_" ++ "main" ++ "_end")]
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

--------------------
-- IR to Assembly --
--------------------

-- Translate the assembly IR returned by codeGen into real assembly
emitASM :: ASMStatement -> Either String String
emitASM (Push r@Reg{}) = Right $ "push " ++ (show r)
emitASM (Pop r@Reg{})  = Right $ "pop " ++ (show r)
emitASM (Add rd@Reg{} rs@Reg{}) = Right $ "add " ++ (show rd) ++ ", " ++ (show rs)
emitASM (Sub rd@Reg{} rs@Reg{}) = Right $ "sub " ++ (show rd) ++ ", " ++ (show rs)
emitASM (Mov rd@Reg{} i@Imm{})  = Right $ "mov " ++ (show rd) ++ ", " ++ (show i)
emitASM (Asp i@Imm{})           = Right $ "asp " ++ (show i)
emitASM (Store src@Reg{} dest@Reg{}) = Right $ "st " ++ (show src) ++ ", " ++ (show dest)
emitASM (Store src@Reg{} dest@(Plus Reg{} Imm{})) = Right $ "st " ++ (show src) ++ ", " ++ (show dest)
emitASM (LabelDef lbl)          = Right $ lbl ++ ":"
emitASM (Rsp r@Reg{})           = Right $ "rsp " ++ (show r)
emitASM (Wsp r@Reg{})           = Right $ "wsp " ++ (show r)
emitASM (Call l@Label{})        = Right $ "call " ++ show l
emitASM (Brn l@Label{})        = Right $ "brn " ++ show l
emitASM Ret                     = Right "ret"
emitASM Nop                     = Right "nop"
emitASM instr = Left $ "Undefined ASMStatement: " ++ (show instr)

compileAST = (fmap emitASM) . codeGen
