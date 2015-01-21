module Hash (runInteractive, runScript) where

import Data.Maybe (fromJust)
import Language.Exec (runHashProgram, wd)
import Language.Commands
import Parsing.HashParser (parseTLExpr)
import System.FilePath (takeDirectory)
import System.Directory (canonicalizePath, getHomeDirectory)
import System.Console.Readline (readline, addHistory)
import System.Environment (getArgs)


runScript :: FilePath -> IO ()
runScript fp = do
  f <- readFile fp
  let tlexprs = parseTLExpr f
  canonFp <- canonicalizePath fp
  runHashProgram commandTable (Left $ takeDirectory $ canonFp) tlexprs
  return ()

runInteractive :: IO ()
runInteractive = do
  home <- getHomeDirectory
  l <- readline (home ++ "$ ")
  let tlexprs = parseTLExpr (fromJust l ++ ";")
  defSs <- runHashProgram commandTable (Left home) tlexprs
  let loop ss = do
        l' <- readline (wd ss ++ "$ ")
        let l = fromJust l'
        addHistory l
        if l == "quit" then return ss 
          else (do
              let tlexprs = parseTLExpr (l++";")
              newSs <- runHashProgram commandTable (Right ss) tlexprs
              loop newSs) 
  loop defSs
  return ()
