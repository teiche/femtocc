module SADeclaration where

import Data.List
import Data.Function
import Data.Maybe

import Syntax

data SemanticError = UseBeforeDeclaration Name
                   | MultipleDeclarations [Symbol]
                     deriving (Show)
            
type Scope = [Symbol]
type SymbolTable = [Scope]

--------------------------------
-- Symbol Table Manipulations --
--------------------------------
    
visible :: SymbolTable -> Name -> Bool
visible st name = name `elem` visibleNames
  where visibleNames = map symbolName (concat st)

-- Given a symbol, determine if it's in the current scope    
inTopScope :: SymbolTable -> Symbol -> Bool
inTopScope st sym = sym `elem` (head st)
    
-- Given a symbol, look it up in the symbol table    
getBySymbol :: SymbolTable -> Symbol -> Maybe Symbol
getBySymbol st sym = listToMaybe $ filter (((==) `on` symbolName) sym) (head st)


                     
-- Return a list of lists of elements that are duplicates as determined by f
dupsBy :: (a -> a -> Bool) -> [a] -> [[a]]
dupsBy f = filter ((>1) . length) . groupBy f

-- Given a list of symbols introduced in the same scope, return any errors
-- arising from duplicate declarations
errorDuplicates :: [Symbol] -> [SemanticError]
errorDuplicates args = fmap MultipleDeclarations $ dupsBy ((==) `on` symbolName) args
                   
-- Verify that all identifiers are declared exactly once before their use
checkDeclarations :: [Expr] -> [SemanticError]
checkDeclarations exprs = checkDeclarations' [] exprs

checkDeclarations' :: SymbolTable -> [Expr] -> [SemanticError]
checkDeclarations' st exprs = concatMap (checkExprDeclarations st) exprs

-- Analyze a single expression for declaration issues, and recurse into child notes
-- FuncDef:
---- At a function definition we enter a new scope
---- So get all symbols defined as arguments, make sure their aren't duplicates,
---- and recurse into child nodes
-- VariableDecl:
---- When a new symbol is declared, make sure it doesn't exist in the current scope
checkExprDeclarations :: SymbolTable -> Expr -> [SemanticError]
checkExprDeclarations st (BinOp _ left right) = (checkExprDeclarations st left) ++
                                                (checkExprDeclarations st right)
checkExprDeclarations st (FuncDef _ _ args children) = localErrors ++ (checkDeclarations'
                                                                       (args : st) children)
  where localErrors = errorDuplicates args
checkExprDeclarations st (FuncCall _ exprs) = checkDeclarations' st exprs
checkExprDeclarations st (VariableDecl sym) = case (getBySymbol st sym) of
                                                Nothing -> []
                                                Just s -> [MultipleDeclarations [sym, s]]
checkExprDeclarations st (If cond body) = (checkExprDeclarations st cond) ++
                                            (checkDeclarations' st body)
checkExprDeclarations st (Const _) = []
checkExprDeclarations st (Assignment e1 e2) = checkDeclarations' st [e1, e2]
checkExprDeclarations st (Identifier name) = if (visible st name) then [] else
                                                 [UseBeforeDeclaration name]
checkExprDeclarations st (Return (Just expr)) = checkExprDeclarations st expr
checkExprDeclarations _ _ = []
