module Hash.Language.Exec where

import Control.Applicative (pure)
import Control.Monad (when, unless, foldM)
import qualified Data.Map as M
import Data.Maybe (maybe, fromMaybe, isNothing, fromJust)
import Hash.Language.Expressions
import Hash.Parsing.HashParser


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

-- unwraps str expressions and substitutes var expressions with their values
varSub :: VarTable -> Expr -> String
varSub vt (Str s) = s
varSub vt (Var s) = fromMaybe (error "variable " ++ s ++ " does not exist\n") $ M.lookup s vt

-- executes a command
executeCmd :: ScriptState -> CommandTable -> Cmd -> IO ScriptState
executeCmd ss ct (Cmd cmdName args input out isAppend) = do
  let vs = varSub (vartable ss)
  let cmdName' = vs cmdName
  let cmd = fromMaybe (error ("command " ++ cmdName' ++ " does not exist\n")) $ M.lookup cmdName' ct
  fromFile <- maybe (pure "" :: IO String) readFile (fmap vs input)
  let args' = args ++ parsed (betterParse (manyExpr) fromFile)
  newSs <- cmd (map vs args') ss

  unless (isNothing out) $ (if isAppend then appendFile else writeFile) (vs $ fromJust out) $ output newSs

  when (isNothing out) $ putStr $ output newSs

  return newSs
  

-- Runs a set of commands for a given command table. If this is the first
-- command in the chain, it is given a FilePath and constructs a new, initially
-- blank, ScriptState. Otherwise, it is given the state as left by the previous
-- command's execution.
runHashProgram :: CommandTable -> Either FilePath ScriptState -> [TLExpr] -> IO ScriptState
runHashProgram _  (Left _)  []         = error "No commands to run"
runHashProgram ct (Left fp) (tle:tles) = do
  newSs <- runTopLevel ct (ScriptState {output = "", wd = fp, vartable = M.empty}) tle
  runHashProgram ct (Right newSs) tles
runHashProgram _  (Right ss) []         = pure ss :: IO ScriptState
runHashProgram ct (Right ss) (tle:tles) = do
  newSs <- runTopLevel ct ss tle
  runHashProgram ct (Right newSs) tles



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
runTopLevel t ss (TLCmd cmd) = do
  let vs = varSub (vartable ss)
  case cmd of
    (Cmd _ _ _ _ _) ->
      executeCmd ss t cmd
    (Assign var val) -> do
      let newVarTable = M.insert (vs var) (vs val) (vartable ss);
      return ss{vartable = newVarTable}
runTopLevel t ss (TLCnd (IfElse pred trueCl falseCl)) = do
  let cmds = (if evalPred ss pred then trueCl else falseCl)
  newSs <- foldM (runTopLevel t) ss (map TLCmd cmds)
  return newSs


  

