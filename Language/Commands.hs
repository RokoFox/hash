module Hash.Language.Commands where

import Control.Monad (void, mapM)
import qualified Data.Map as M
import Data.List
import Data.Maybe (fromMaybe)
import Hash.Language.Exec (CommandTable, ScriptState, VarTable, output, vartable)
import System.Process (rawSystem)
import System.Environment (getArgs)
import System.Directory (removeFile)

commandDir = "/media/windows/Haskell/Projekt/Hash/Language/CommandExes/"

type Command = [String] -> ScriptState -> IO ScriptState

commandTable :: CommandTable
commandTable = M.fromList [
  ("echo", echo),
  ("rm"  , rm)]


rm :: Command
rm args ss = do
  let dirs = map (subVarFromLit (vartable ss)) args
  mapM removeFile dirs
  return ss {output = ""}

echo :: Command
echo args ss = return (ss {output = (intercalate " " $ map (subVarFromLit (vartable ss)) args) ++ "\n"})


subVarFromLit :: VarTable -> String -> String
subVarFromLit vt str =  concatMap (\(f,s) -> if odd s then subVars f else f) $ findVars str 0
  where findVars "" i = [("",i)]
        findVars (c:str) i
          | c == '$'  = ("", i) : findVars str (i+1)
          | otherwise = let ((h,ind):t) = findVars str i in (((c:h),ind):t)
        subVars str = fromMaybe (error "variable " ++ str ++ 
                         " does not exist\n") $ M.lookup str vt
