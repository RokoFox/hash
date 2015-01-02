module Hash.Language.Commands where

import Control.Monad (void, mapM)
import qualified Data.Map as M
import Data.List
import Data.Maybe (fromMaybe)
import Hash.Language.Exec (CommandTable, ScriptState, VarTable, 
                           subVarFromLit, output, vartable, wd, absolutePath)
import System.Process (rawSystem)
import System.Environment (getArgs)
import System.Directory
import System.FilePath

type Command = [String] -> ScriptState -> IO ScriptState

commandTable :: CommandTable
commandTable = M.fromList [
  ("ls"     , ls),
  ("cd"     , cd),
  ("pwd"    , pwd),
  ("cpdir"  , cpdir),
  ("mkdir"  , mkdir),
  ("rmdir"  , rmdir),
  ("mv"     , mv),
  ("cp"     , cp),
  ("rm"     , rm),
  ("create" , create),
  ("echo"   , echo)]

ls :: Command
ls args ss = do
  arg <- preparePaths ss args
  conts <- getDirectoryContents (if arg == [] then wd ss else head arg)
  let maxLen = maximum $ map length conts
  let colSize = div 80 (div 80 maxLen)
  return ss {output = concatMap (\str -> str++(replicate (colSize - length str) ' ')) conts ++ "\n"}

cd :: Command
cd []      ss = do
  home <- getHomeDirectory
  return (ss {output = "", wd = home})
cd (arg:_) ss = do
  newWd <- canonicalizePath (wd ss </> arg)
  exists <- doesDirectoryExist newWd
  if exists then return (ss {output = "", wd = newWd}) 
            else error (arg ++ ": no such file or directory")

pwd :: Command
pwd _ ss = return (ss {output = wd ss ++ "\n"})
   

cpdir :: Command
cpdir args ss = do
  dirs <- preparePaths ss args
  let dest = last dirs
  exists <- doesDirectoryExist dest
  let copy a b = do
        createDirectory b
        cont' <- getDirectoryContents a
        let cont = map ((a++"/")++) $ drop 2 cont'
        types <- mapM doesFileExist cont
        if cont == [] then return () else void $ mapM (\(src, t) -> if t
                           then copyFile src (b </>  takeBaseName src)
                           else copy src (b </>  takeBaseName src)) $ zip cont types
        return ()
  if exists then mapM (\src -> copy src (dest </>  takeBaseName src)) (init dirs)   
            else mapM (flip copy dest) (init dirs)
  return ss {output = ""}

mkdir :: Command
mkdir args ss = do
  dirs <- preparePaths ss args
  mapM createDirectory dirs
  return ss {output = ""}

rmdir :: Command
rmdir args ss = do
  let (fs, dirs') = flags args
  let rec = elem "r" fs
  dirs <- preparePaths ss dirs'
  mapM (if rec then removeDirectoryRecursive else removeDirectory) dirs
  return ss {output = ""}
  

mv :: Command
mv args ss = do
  dirs <- preparePaths ss args
  let dest = last dirs
  exists <- doesDirectoryExist dest
  let rename a b = do
        dirEx <- doesDirectoryExist a
        fileEx <- doesFileExist a
        let ret
              | dirEx     = renameDirectory a 
              | fileEx    = renameFile a
              | otherwise = error ("No directory or file :" ++ show a)
        ret b
  if exists then mapM (\src -> rename src (dest </>  takeBaseName src)) (init dirs)   
            else mapM (flip renameFile dest) (init dirs)
  return ss {output = ""}

cp :: Command
cp args ss = do
  dirs <- preparePaths ss args
  let dest = last dirs
  exists <- doesDirectoryExist dest
  if exists then mapM (\src -> copyFile src (dest </>  takeBaseName src)) (init dirs)   
            else mapM (flip copyFile dest) (init dirs)
  return ss {output = ""}

rm :: Command
rm args ss = do
  dirs <- preparePaths ss args
  mapM removeFile dirs
  return ss {output = ""}

create :: Command
create args ss = do
  dirs <- preparePaths ss args
  mapM (flip appendFile "") dirs
  return ss {output = ""}
  

echo :: Command
echo args ss = return (ss {output = (intercalate " " $ map (subVarFromLit (vartable ss)) args) ++ "\n"})


preparePaths :: ScriptState -> [FilePath] -> IO [FilePath]
preparePaths ss fps = do
  let dirs' = map (subVarFromLit (vartable ss)) fps
  mapM (absolutePath (wd ss)) dirs'


flags :: [String] -> ([String], [String])
flags args = (map tail f, s)
  where (f, s) = partition ((=='-') . head) args

 
