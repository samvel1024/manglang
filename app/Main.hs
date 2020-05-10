module Main where

import           Control.Monad      (when)
import           System.Environment (getArgs, getProgName)
import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (hGetContents, stdin)

import           LexDeclaration
import           ParDeclaration

import           ErrM



main :: IO ()
main =
  case pProgram (myLexer "example.lang") of
    Bad err -> do
      putStrLn "Syntax error:"
      putStrLn err
      exitFailure
    Ok tree -> do
      putStrLn $ show tree
