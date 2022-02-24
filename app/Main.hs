module Main where

import System.Environment (getArgs)

import Lib

main :: IO ()
main =
  do args <- getArgs
     bindings <- processFiles True args
     repl bindings
