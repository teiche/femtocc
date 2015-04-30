module SADeclaration where

import Data.List
import Data.Function
import Data.Maybe

import Syntax

data SemanticError = UseBeforeDeclaration
                   | MultipleDeclarations [Symbol]
                     deriving (Show)
            
type Scope = [Symbol]
type SymbolTable = [Scope]

-- Given a symbol, determine if it's in the current scope    
inScope :: SymbolTable -> Symbol -> Bool
inScope st sym = sym `elem` (head st)
    
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

-- Perform analysis on a single expression                              
checkExprDeclarations :: SymbolTable -> Expr -> [SemanticError]
checkExprDeclarations st (BinOp _ left right) = (checkExprDeclarations st left) ++
                                                (checkExprDeclarations st right)
-- At a function definition we enter a new scope
-- So get all symbols defined as arguments, make sure their aren't duplicates,
-- and recurse into child nodes
checkExprDeclarations st (FuncDef _ _ args children) = localErrors ++ (checkDeclarations'
                                                                          (args : st) children)
  where localErrors = errorDuplicates args
checkExprDeclarations st (FuncCall _ exprs) = checkDeclarations' st exprs
-- When a new symbol is declared, make sure it doesn't exist in the current scope
checkExprDeclarations st (VariableDecl sym) = case (getBySymbol st sym) of
                                                Nothing -> []
                                                Just s -> [MultipleDeclarations [sym, s]]
checkExprDeclarations _ _ = []                                                          
