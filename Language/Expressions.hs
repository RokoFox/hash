module Language.Expressions where



data Cmd = Cmd { name   :: Expr -- The command name (can be a variable)
               , args   :: [Expr] -- The command arguments
               , inDir  :: Maybe Expr -- A redirected input fp
               , outDir :: Maybe Expr -- A redirected output fp
               , append :: Bool -- If redirected, is it appending?
               }
         | Assign { var :: Expr -- Assignment target
                  , val :: Expr -- A value to assign to a variable
                  } deriving Show



data Expr = Var String -- A named variable
          | Str String -- A mere string, the peasant of expressions
            deriving (Eq, Show)

data Comp = CEQ Expr Expr -- ==
          | CNE Expr Expr -- /=
          | CGE Expr Expr -- >=
          | CGT Expr Expr -- >
          | CLE Expr Expr -- <=
          | CLT Expr Expr -- <
          | CLI Expr -- A wrapped expression literal - True if nonempty
            deriving (Eq, Show)


data Pred = Pred Comp -- A wrapped comparison
          | Not Pred -- Negation
          | And Pred Pred -- A binary logical and
          | Or Pred Pred -- A binary logical or
          | Parens Pred -- An expression in parentheses
            deriving (Eq, Show)

data ArithExpr = Val Expr
               | Neg ArithExpr
               | Plus ArithExpr ArithExpr
               | Minus ArithExpr ArithExpr
               | Mul ArithExpr ArithExpr
               | Div ArithExpr ArithExpr
               | Mod ArithExpr ArithExpr
               | Exp ArithExpr ArithExpr
               | ParensAE ArithExpr
                 deriving (Eq, Show)


data Conditional = IfElse { cond  :: Pred -- Predicate to satisfy
                          , cthen :: [TLExpr] -- Actions if satisfied
                          , celse :: [TLExpr] -- Actions otherwise
                      }
                        deriving Show

data WhileLoop = WhileLoop { condW :: Pred
                           , cmds :: [TLExpr] } deriving Show

data TLExpr = TLCmd  Cmd
            | TLCnd  Conditional
            | TLLoop WhileLoop  
               deriving Show
