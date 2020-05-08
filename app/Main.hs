module Main where

import           Control.Monad      (when)
import           System.Environment (getArgs, getProgName)
import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (hGetContents, stdin)

import           LexDeclaration
import           ParDeclaration

import           ErrM


usage = do
  putStrLn "provide an input file name as an argument"
  exitFailure

main :: IO ()
main = putStrLn "HELLO WORLD"
