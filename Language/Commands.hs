module Language.Commands where

import Control.Monad (void, mapM)
import qualified Data.Map as M
import qualified Data.ByteString as BS
import Data.Word
import Data.List
import Data.Char (ord, intToDigit, toLower)
import Data.Maybe (fromMaybe)
import Language.Exec (CommandTable, ScriptState, VarTable, 
                           subVarFromLit, output, vartable, wd, absolutePath)
import System.Process (rawSystem)
import System.Environment (getArgs)
import System.Directory
import System.FilePath
import System.Console.Terminal.Size

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
  ("cat"    , cat),
  ("echo"   , echo),
  ("hexdump", hexdump),
  ("grep"   , grep)]

ls :: Command
ls args ss = do
  arg <- preparePaths ss args
  conts <- getDirectoryContents (if arg == [] then wd ss else head arg)
  let colSize = 1 + (maximum $ map length conts)
  w <- size
  let wid = width $ fromMaybe (Window 0 80) w 
  let pad str = str++(replicate (colSize - length str) ' ')
  let out (str:strs) i 
        | (i + colSize > wid)  = "\n"++ pad str ++ out strs colSize
        | otherwise            = pad str ++ out strs (i+colSize)
      out _          _         = ""
  return ss {output = out (sort conts) 0 ++ "\n"}

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
  paths <- preparePaths ss args
  let dest = last paths
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
  if exists then mapM_ (\src -> copy src (dest </>  takeBaseName src)) (init paths)   
            else mapM_ (flip copy dest) (init paths)
  return ss {output = ""}

mkdir :: Command
mkdir args ss = do
  paths <- preparePaths ss args
  mapM_ createDirectory paths
  return ss {output = ""}

rmdir :: Command
rmdir args ss = do
  let (fs, paths') = flags args
  let rec = elem "r" fs
  paths <- preparePaths ss paths'
  mapM_ (if rec then removeDirectoryRecursive else removeDirectory) paths
  return ss {output = ""}
  

mv :: Command
mv args ss = do
  paths <- preparePaths ss args
  let dest = last paths
  exists <- doesDirectoryExist dest
  let rename a b = do
        dirEx <- doesDirectoryExist a
        fileEx <- doesFileExist a
        let ret
              | dirEx     = renameDirectory a 
              | fileEx    = renameFile a
              | otherwise = error ("No directory or file :" ++ show a)
        ret b

  if exists then mapM_ (\src -> rename src (dest </>  takeBaseName src)) (init paths)   
            else mapM_ (flip rename dest) (init paths)
  return ss {output = ""}

cp :: Command
cp args ss = do
  paths <- preparePaths ss args
  let dest = last paths
  exists <- doesDirectoryExist dest
  if exists then mapM_ (\src -> copyFile src (dest </>  takeBaseName src)) (init paths)   
            else mapM_ (flip copyFile dest) (init paths)
  return ss {output = ""}

rm :: Command
rm args ss = do
  paths <- preparePaths ss args
  mapM removeFile paths
  return ss {output = ""}

create :: Command
create args ss = do
  paths <- preparePaths ss args
  mapM (flip appendFile "") paths
  return ss {output = ""}


cat :: Command
cat args ss = do
  paths <- preparePaths ss args
  fs <- mapM readFile paths
  return ss {output = intercalate "\n" fs}
  

echo :: Command
echo args ss = return (ss {output = 
  (intercalate " " $ map (subVarFromLit (vartable ss)) args) ++ "\n"})


hexdump :: Command
hexdump args ss = do
  (fp:fps) <- preparePaths ss args
  f <- BS.readFile fp
  let h = columnize 40 '\n' $ columnize 4 ' ' $ concat $ map hex $ BS.unpack f
  return (ss {output = h ++ "\n"})

grep :: Command
grep args ss = do
  let (fls, (str:fps')) = flags args
  (fp:_) <- preparePaths ss fps'
  let v = elem "v" fls
  let i = elem "i" fls
  let o = elem "o" fls
  let n = elem "n" fls
  let c = elem "c" fls
  let isInfixOf' xs ys = isInfixOf (map toLower xs) $ map toLower ys
  let matchi = if i then isInfixOf' else isInfixOf
  let match = if v then not . matchi str else matchi str
  cont <- readFile fp
  let ret
        | o = findInfixes str cont
        | n = map (\(i, l) -> show i ++ ":" ++ l) $ 
                        filter (match . snd) $ zip [1..] $ lines cont
        | otherwise = filter match $ lines cont
  let out = (if c then show $ length ret else unlines ret) ++ "\n"
  return ss {output = out}

findInfixes :: Eq a => [a] -> [a] -> [[a]]
findInfixes inf [] = []
findInfixes inf l@(_:xs) = if isPrefixOf inf l then inf:(findInfixes inf xs)
                                             else findInfixes inf xs

hex :: Word8 -> String
hex c = [intToDigit $ fromInteger d, intToDigit $ fromInteger m]
  where (d,m) = (`divMod` 16) $ toInteger c

columnize :: Int -> Char -> String -> String
columnize n = columnize' n n
 where columnize' _ _ _ ""       = ""
       columnize' 0 n c str      = c:(columnize' n n c str)
       columnize' i n c (c':str) = c':(columnize' (i-1) n c str)

preparePaths :: ScriptState -> [FilePath] -> IO [FilePath]
preparePaths ss fps = do
  let paths' = map (subVarFromLit (vartable ss)) fps
  mapM (absolutePath (wd ss)) paths'


flags :: [String] -> ([String], [String])
flags args = (map tail f, s)
  where (f, s) = partition ((=='-') . head) args

 
