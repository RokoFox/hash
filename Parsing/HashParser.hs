module Hash.Parsing.HashParser where

import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, char, string, spaces, letter, satisfy, alphaNum, anyChar, noneOf, oneOf)
import Text.Parsec (parse, ParseError, try, optionMaybe)
import Text.Parsec.Combinator (many1, sepBy1)
import Text.Parsec.Expr
import Control.Applicative ((<|>), (<$>), (<$), (<*>), (<*), (*>), Applicative, many)
import Control.Monad (when)
import Data.Char (digitToInt, isAlphaNum, isLetter)
import Data.Maybe (isNothing, fromMaybe)
import Hash.Language.Expressions

err = "An error has occurred"

infixr 5 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a] 
a <:> b = (:) <$> a <*> b

token :: Parser a -> Parser a
token = (<* spaces)

parseTLExpr = parsed . betterParse (many tlexpr)

parsed (Right s) = s
parsed (Left _)  = error err

betterParse :: Parser a -> String -> Either ParseError a
betterParse p = parse (spaces *> p) err

anyString :: Parser String
anyString = token $ many1 (noneOf ['=', ' ', '<', '>', '(', ')', '\n', ';', '{', '}', '#'])

anyLitString :: Parser String
anyLitString = token $ many (noneOf ['"'])

stringLiteral :: Parser Expr
stringLiteral = do
  token $ char '"'
  ret <- Str <$> anyLitString
  token $ char '"'
  return ret

strExpr :: Parser Expr
strExpr = Str <$> anyString

variable :: Parser String
variable = do
  char '$'
  first <- satisfy (\c -> isLetter c || c == '_')
  rest <- many (satisfy (\c -> isAlphaNum c || c == '_'))
  spaces
  return (first:rest)

varExpr :: Parser Expr
varExpr = Var <$> variable

expr :: Parser Expr
expr = try varExpr <|> strExpr

manyExpr = many expr

symbol :: Char -> Parser Char
symbol = token . char

assign :: Parser Cmd
assign = do
  varName <- strExpr
  char '='
  value <- strExpr
  spaces
  symbol ';'
  return (Assign varName value)
  

inputRedir :: Parser Expr
inputRedir = do
  char '<'
  spaces
  redirExpr <- (try stringLiteral <|> expr)
  return redirExpr
  
outputRedir :: Parser (Expr, Bool)
outputRedir = do
  char '>'
  appended <- optionMaybe (char '>')
  spaces
  redirExpr <- (try stringLiteral <|> expr)
  spaces
  return (redirExpr, not $ isNothing appended)
 

cmd :: Parser Cmd
cmd = do
  cmdName <- expr
  spaces
  cmdArgs <- many (try stringLiteral <|> expr)
  spaces
  redirIn <- optionMaybe inputRedir
  redirOut <- optionMaybe outputRedir
  spaces
  let (outputExpr, isAppended) = case redirOut of
                                   Nothing          -> (Nothing, False)
                                   Just (ex, isApp) -> (Just ex, isApp)
  symbol ';'                                 
  return (Cmd cmdName cmdArgs redirIn outputExpr isAppended)
  
  
cmdOrAssign :: Parser Cmd
cmdOrAssign = try assign <|> cmd
    
comp :: Parser Comp
comp = do
  expr1 <- expr
  spaces
  op <- many1 (oneOf ['=', '/', '>', '<'])
  spaces
  expr2 <- expr
  spaces
  let opConst = case op of
                    "==" -> CEQ
                    "/=" -> CNE
                    ">=" -> CGE
                    ">"  -> CGT
                    "<=" -> CLE
                    "<"  -> CLT
  return (opConst expr1 expr2)


table = [[unary '!' Not], [binary '&' And], [binary '|' Or]]
  where binary sym f = Infix (mkParser sym f) AssocLeft
        mkParser s f = do
          char s
          spaces
          return f
        unary sym f = Prefix (mkParser sym f)

prd :: Parser Pred
prd = buildExpressionParser table other
  where other = cmp <|> parenPred
        cmp = do
          c <- comp
          spaces
          return (Pred c)
        parenPred = do
          char '('
          pr <- prd
          char ')'
          spaces
          return (Parens pr)

clause :: Parser [Cmd]
clause = do
  symbol '{'
  cmds <- many cmdOrAssign
  symbol '}'
  return (cmds)


conditional :: Parser Conditional
conditional = do
  string "if"
  spaces
  char '('
  cnd <- prd
  spaces
  char ')'
  spaces
  cmds1 <- clause
  spaces
  el <- optionMaybe $ string "else"
  spaces
  cmds2 <- optionMaybe clause
  spaces
  fi <- string "fi"
  spaces
  return (IfElse cnd cmds1 $ fromMaybe [] cmds2)

comment :: Parser ()
comment = do
  symbol '#'
  token $ many (noneOf ['#'])
  symbol '#'
  return ()
  

tlexpr :: Parser TLExpr
tlexpr = many comment *> (try (TLCmd <$> cmdOrAssign) <|> (TLCnd <$> conditional))
  
  


          
         
 






        
          
     
