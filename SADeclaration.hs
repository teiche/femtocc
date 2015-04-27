module SADeclaration where

data SemanticError = UseBeforeDeclaration


data Symbol = Symbol Identifier Type
            
type Scope = [Symbol]            
type SymbolTable = [Scope]
                    
                   
-- Verify that all identifiers are declared before their use
checkDeclarations :: [Expr] -> [SemanticError]
checkDeclarations root = checkDeclarations root []

checkDeclarations' :: SymbolTable -> Expr -> [SemanticError]
checkDeclarations' st (FuncDef _ vars)
