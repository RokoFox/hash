module Main where

import Hash
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args then runInteractive
               else runScript $ head args
