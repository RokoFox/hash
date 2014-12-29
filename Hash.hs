import Hash.Language.Exec (runHashProgram)
import Hash.Language.Commands
import Hash.Parsing.HashParser (parseTLExpr)


runScript :: FilePath -> IO ()
runScript fp = do
  f <- readFile fp
  let tlexprs = parseTLExpr f
  runHashProgram commandTable (Left fp) tlexprs
  return ()
