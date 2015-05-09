module Main where

import System.Environment
import Data.List (intercalate)
import Data.Either (partitionEithers)

import Text.ParserCombinators.Parsec
    
import Parser
import SADeclaration
import FrameOffsetTable
import BEStackMachine

-- UNCOMMENT FOR REPL-like THING
{- 
import Control.Monad.Trans
import System.Console.Haskeline

process :: String -> IO ()
process line = do
  let res = parseFuncDef line
  print res


main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "HCLex> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop
-}

main :: IO()
main = do
     args <- getArgs
     ast <- parseFromFile program (args !! 0)

     print $ fmap checkDeclarations ast
     print ast

     case ast of
       Left err   -> error (show err)
       Right body -> do
                       print $ generateFunctionOffset (head body)
                       let (errors, code) = partitionEithers $ compileAST (head body)
                       print errors
                       putStrLn $ intercalate "\n" code
