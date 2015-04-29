module SADeclaration where

import Data.List

import Syntax

data SemanticError = UseBeforeDeclaration
            
type Scope = [Symbol]
type SymbolTable = [Scope]

-- Return a list of lists of elements that are duplicates as determined by f
dupsBy :: (a -> a -> Bool) -> [a] -> [[a]]
dupsBy f = filter ((>1) . length) . groupBy f

-- Given a list of symbols introduced in the same scope, return any errors
-- arising from duplicate declarations
errorDuplicates :: [Symbol] -> [SemanticError]
errorDuplicates = undefined
                   
-- Verify that all identifiers are declared before their use
checkDeclarations :: [Expr] -> [SemanticError]
checkDeclarations root = checkDeclarations' [] root

checkDeclarations' :: SymbolTable -> Expr -> [SemanticError]
-- At a function definition we enter a new scope
-- So get all symbols defined as arguments, make sure their aren't duplicates,
-- and recurse into child nodes
checkDeclarations' st (FuncDef _ _ args children) = localErrors ++ (checkDeclarations'
                                                                    (st ++ args) children)
  where localErrors = errorDuplicates args
