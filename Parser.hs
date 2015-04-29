-- Parser/Lexer for a small subset of the C language
-- Written by Alex Teiche February 2015
-- Based on Stephen Diehl's "Write You a Haskell" parser in Chapter 3

module Parser where

import Numeric
import Control.Monad
import Control.Applicative ((<$>))
import Data.Char

    
import Syntax

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Data.Functor.Identity

reservedNames :: [String]
reservedNames = ["int", "return"]

reservedOpNames :: [String]
reservedOpNames = ["-", "==", "=", "*"]

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
        { Tok.commentStart    = "/*"
        , Tok.commentEnd      = "*/"
        , Tok.commentLine     = "//"
        , Tok.nestedComments  = True
        , Tok.identStart      = letter
        , Tok.identLetter     = alphaNum <|> char '_'
        , Tok.opStart         = oneOf "+-=*"
        , Tok.opLetter        = oneOf "=*"
        , Tok.reservedNames   = reservedNames
        , Tok.reservedOpNames = reservedOpNames
        , Tok.caseSensitive   = True
        }
                
lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

semi = Tok.semi lexer
comma = Tok.comma lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

commaSep = Tok.commaSep lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

--identifier :: String -> Parser String
identifier = Tok.identifier lexer

symbol = Tok.symbol lexer

binaryOp :: String -> Ex.Assoc -> Ex.Operator String () Identity Expr
binaryOp s assoc = Ex.Infix (reservedOp s >> return (BinOp s)) assoc

lassocOp :: String -> Ex.Operator String () Identity Expr
lassocOp s = binaryOp s Ex.AssocLeft

-- Precedence levels taken from this table:
-- http://en.wikipedia.org/wiki/Operators_in_C_and_C%2B%2B#Operator_precedence

opTable :: Ex.OperatorTable String () Identity Expr
opTable = [ [
          -- C Precedence Level 6
          lassocOp "+"
        , lassocOp "-"
        ] , [
          -- C Precedence Level 7
          lassocOp "=="
        ] , [
          -- C Precedence Level 16
          lassocOp "="
        ] ]
          
term :: Parser Expr
term =
         int
     <|> parens expr
     <|> try functionCall
     <|> (identifier >>= (return . Identifier))


expr :: Parser Expr
expr = Ex.buildExpressionParser opTable term

functionCall :: Parser Expr
functionCall = do
             name <- identifier
             args <- parens (commaSep expr)
             return $ FuncCall name args

-- END EXPRESSION PARSER, BEGIN AST PARSER


translationUnit :: Parser Expr
translationUnit = functionDefinition

functionDefinition :: Parser Expr
functionDefinition = do
                   retType <- fullType
                   name <- identifier
                   arguments <- parens (commaSep variable)
                   body <- braces statements
                   return $ FuncDef name retType arguments body

statement :: Parser Expr
statement =   selectionStatement
          <|> jumpStatement
          <|> expressionStatement
          <|> assignment
          <|> variableDeclaration

statements :: Parser [Expr]
--statements = statement `sepEndBy` semi
statements = many statement

assignment :: Parser Expr
assignment = do
           lval <- expr
           reservedOp "="
           rval <- expr
           return $ Assignment lval rval

selectionStatement :: Parser Expr
selectionStatement = ifClause

jumpStatement :: Parser Expr
jumpStatement = do
              reserved "return"
              result <- optionMaybe expr
              semi
              return $ Return result
          
expressionStatement :: Parser Expr
expressionStatement = (semi >> return Nop) <|> do
                    val <- expr
                    semi
                    return val

ifClause :: Parser Expr
ifClause = do
         reserved "if"
         cond <- parens expr
         body <- braces statements
         return $ If cond body

variableDeclaration :: Parser Expr
variableDeclaration = VariableDecl <$> variable

variable :: Parser Symbol
variable = do
         typ <- fullType
         name <- identifier
         return $ Symbol name typ

primType :: Parser PrimType
primType = reserved "int" >> return Int

fullType :: Parser Type
fullType = do
         prim <- primType
         asterisks <- many (symbol "*")
         return $ Type prim (length asterisks)


-- END GRAMMAR

contents :: Parser a -> Parser a
contents p = do
         Tok.whiteSpace lexer
         r <- p
         eof
         return r


program = many translationUnit

-- Number Parsing

int :: Parser Expr
int = try binary <|> try hexadecimal <|> try octal <|> decimal

decimal :: Parser Expr
decimal = liftM (Const . read) $ many1 digit

hexadecimal :: Parser Expr
hexadecimal = liftM (Const . extractNum . readHex) $ hexPrefix >> (many1 hexDigit)
            where hexPrefix = (try (string "0x") <|> string "0X")

octal :: Parser Expr
octal = liftM (Const . extractNum . readOct) $ octalPrefix >> (many1 octDigit)
      where octalPrefix = char '0'

binary :: Parser Expr
binary = liftM (Const . readBin) $ binaryPrefix >> (many1 binDigit)
       where binaryPrefix = string "0b"
             binDigit = oneOf "01"

-- Extract the number from the result of readHex and readOctal
extractNum :: [(Integer, String)] -> Integer
extractNum = fst . head

readBin :: Integral a => String -> a
readBin s = (fst . head) $ readInt 2 (`elem` "01") digitToInt s
