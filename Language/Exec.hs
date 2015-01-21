module Language.Exec where

import Control.Applicative (pure)
import Control.Monad (when, unless, foldM)
import qualified Data.Map as M
import Data.Maybe (maybe, fromMaybe, isNothing, fromJust)
import Language.Expressions
import Parsing.HashParser
import System.Directory (canonicalizePath)
import System.FilePath ((</>), isRelative, splitFileName)



-- A model of a command which is waiting for arguments and a state to run
type Command = [String] -> ScriptState -> IO ScriptState

-- A table of variables, in fact a map of (Name, Value) pairs.
type VarTable = M.Map String String

-- A command table - abstracted command execution, (contains command name,
-- command) pairs. Simplest, but hardly the best way to implement this.
type CommandTable = M.Map String Command

-- A script state containing the last output, current working directory and
-- the current table of variables.
data ScriptState = ScriptState { output :: String
                                   , wd :: FilePath
                             , vartable :: VarTable
} deriving Show

-- unwraps Str expressions and substitutes Var expressions with their values
varSub :: VarTable -> Expr -> String
varSub vt (Str s) = s
varSub vt (Var s@(h:t))
  | h == '['  = show $ evalArith vt (parseArithExpr t)
  | otherwise = fromMaybe (error ("variable " ++ s ++ " does not exist\n")) $ M.lookup s vt

-- executes a command
executeCmd :: ScriptState -> CommandTable -> Cmd -> IO ScriptState
executeCmd ss ct (Cmd cmdName args input out isAppend) = do
  let vs = varSub (vartable ss)
  let cmdName' = vs cmdName
  let prepareFP fp = do
        a <- absolutePath (wd ss) $ vs fp
        return $ subVarFromLit (vartable ss) a

  let cmd = fromMaybe (error ("command " ++ cmdName' ++ " does not exist\n")) $ M.lookup cmdName' ct
  canonInput <- if isNothing input then pure Nothing :: IO (Maybe FilePath) 
                                   else fmap Just $ prepareFP (fromJust input)
  fromFile <- maybe (pure "" :: IO String) readFile canonInput
  let argsff = parseArgs fromFile
  let args' = args ++ if fromFile == "" then [] else argsff
  newSs <- cmd (map vs args') ss

  if isNothing out then putStr $ output newSs
    else (do
      canonOut <- prepareFP $ fromJust out
      (if isAppend then appendFile else writeFile) canonOut $ output newSs)
  return newSs
  

-- Runs a set of commands for a given command table. If this is the first
-- command in the chain, it is given a FilePath and constructs a new, initially
-- blank, ScriptState. Otherwise, it is given the state as left by the previous
-- command's execution.
runHashProgram :: CommandTable -> Either FilePath ScriptState -> [TLExpr] -> IO ScriptState
runHashProgram _  (Left fp)  []         = return (ScriptState {output = "", wd = fp, vartable = M.empty})
runHashProgram ct (Left fp) (tle:tles) = do
  newSs <- runTopLevel ct (ScriptState {output = "", wd = fp, vartable = M.empty}) tle
  runHashProgram ct (Right newSs) tles
runHashProgram _  (Right ss) []         = pure ss :: IO ScriptState
runHashProgram ct (Right ss) (tle:tles) = do
  newSs <- runTopLevel ct ss tle
  runHashProgram ct (Right newSs) tles


evalArith :: VarTable -> ArithExpr -> Int
evalArith vt expr = let eA = evalArith vt in case expr of
    Val e -> read $ varSub vt e :: Int
    Neg e -> - eA e
    Plus e1 e2 -> eA e1 + eA e2
    Minus e1 e2 -> eA e1 - eA e2
    Mul e1 e2 -> eA e1 * eA e2
    Div e1 e2 -> div (eA e1) $ eA e2
    Mod e1 e2 -> mod (eA e1) $ eA e2
    Exp e1 e2 -> eA e1 ^ eA e2
    ParensAE e -> eA e

evalComp :: ScriptState -> Comp -> Bool
evalComp ss c = let vs = varSub (vartable ss) in case c of
             CEQ e1 e2 -> (read $ vs e1 :: Double) == (read $ vs e2 :: Double)
             CNE e1 e2 -> (read $ vs e1 :: Double) /= (read $ vs e2 :: Double)
             CLT e1 e2 -> (read $ vs e1 :: Double) <  (read $ vs e2 :: Double)
             CLE e1 e2 -> (read $ vs e1 :: Double) <= (read $ vs e2 :: Double)
             CGT e1 e2 -> (read $ vs e1 :: Double) >  (read $ vs e2 :: Double)
             CGE e1 e2 -> (read $ vs e1 :: Double) >= (read $ vs e2 :: Double)
               
             

evalPred :: ScriptState -> Pred -> Bool
evalPred ss p = case p of
             Parens p  -> evalPred ss p
             Not p     -> not $ evalPred ss p
             And p1 p2 -> evalPred ss p1 && evalPred ss p2
             Or p1 p2  -> evalPred ss p1 || evalPred ss p2
             Pred c    -> evalComp ss c

-- Calculates the result of a top-level command execution
runTopLevel :: CommandTable -> ScriptState -> TLExpr -> IO ScriptState
runTopLevel t ss tlexpr = do
  case tlexpr of
    (TLCmd cmd) -> do
      let vs = varSub (vartable ss)
      case cmd of
        (Cmd _ _ _ _ _) ->
          executeCmd ss t cmd
        (Assign var val) -> do
          let newVarTable = M.insert (vs var) (vs val) (vartable ss);
          return ss{vartable = newVarTable}

    (TLCnd (IfElse pred trueCl falseCl)) -> do

      let cmds = if evalPred ss pred then trueCl else falseCl
      newSs <- foldM (runTopLevel t) ss cmds
      return newSs

    (TLLoop (WhileLoop pred block)) -> do
      
      let loop ss = do
            newSs <- foldM (runTopLevel t) ss block
            if evalPred newSs pred then loop newSs else return newSs
      if evalPred ss pred then loop ss else return ss

absolutePath :: FilePath -> FilePath -> IO FilePath
absolutePath wDir fp = do
  let (dir, fn) = splitFileName fp
  canonDir <- if isRelative dir then canonicalizePath (wDir </> dir) else canonicalizePath dir
  return (canonDir </> fn)   


subVarFromLit :: VarTable -> String -> String
subVarFromLit vt str =  concatMap (\(f,s) -> if odd s then subVars f else f) $ findVars str 0
  where findVars "" i = [("",i)]
        findVars (c:str) i
          | c == '$'  = ("", i) : findVars str (i+1)
          | otherwise = let ((h,ind):t) = findVars str i in (((c:h),ind):t)
        subVars str = fromMaybe (error "variable " ++ str ++ 
                         " does not exist\n") $ M.lookup str vt

