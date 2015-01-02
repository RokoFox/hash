import Hash.Language.Exec (runHashProgram)
import Hash.Language.Commands
import Hash.Parsing.HashParser (parseTLExpr)
import System.FilePath (takeDirectory)
import System.Directory (canonicalizePath)


runScript :: FilePath -> IO ()
runScript fp = do
  f <- readFile fp
  let tlexprs = parseTLExpr f
  canonFp <- canonicalizePath fp
  runHashProgram commandTable (Left $ takeDirectory $ canonFp) tlexprs
  return ()
