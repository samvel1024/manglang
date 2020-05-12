module Main where

import           Control.Monad      (when)
import           System.Environment (getArgs, getProgName)
import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (hGetContents, stdin)

import           LexDeclaration
import           ParDeclaration

import           ErrM
import           Eval

-- TODO extract from args
main :: IO ()
main = interpretFile "example.lang"

interpretFile :: String -> IO ()
interpretFile file = do
  fileContent <- readFile file
  case pProgram (myLexer fileContent) of
    Bad err -> do
      putStrLn err
      exitFailure
    Ok tree -> interpret tree
