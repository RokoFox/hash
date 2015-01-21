module Parsing.HashParser where

import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, char, string, spaces, letter, satisfy, alphaNum, anyChar, noneOf, oneOf)
import Text.Parsec (parse, ParseError, try, optionMaybe)
import Text.Parsec.Combinator (many1, sepBy1, choice, optional)
import Text.Parsec.Expr
import Control.Applicative ((<|>), (<$>), (<$), (<*>), (<*), (*>), Applicative, many)
import Control.Monad (when)
import Data.Char (digitToInt, isAlphaNum, isLetter)
import Data.Maybe (isNothing, fromMaybe)
import Language.Expressions

err = "Parse error"

infixr 5 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a] 
a <:> b = (:) <$> a <*> b

token :: Parser a -> Parser a
token = (<* spaces)

parseTLExpr = parsed . betterParse (many tlexpr)

parseArithExpr = parsed . betterParse arith

parseArgs = parsed . betterParse (many (try stringLiteral <|> expr))

parsed (Right s) = s
parsed (Left pe) = error (show pe) 

betterParse :: Parser a -> String -> Either ParseError a
betterParse p = parse (spaces *> p) err

anyString :: Parser String
anyString = token $ many1 (noneOf ['=', ' ', '<', '>', '(', ')',
                                  '\n', ';', '{', '}', '#',
                                  '+', '*', '%'])

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

arithExpr :: Parser String
arithExpr = do
  token $ string "$"
  h <- char '['
  ret <- many (noneOf [']'])
  symbol ']'
  return (h:ret)

varExpr :: Parser Expr
varExpr = Var <$> (try variable <|> arithExpr)

expr :: Parser Expr
expr = try varExpr <|> strExpr

manyExpr = many expr

symbol :: Char -> Parser Char
symbol = token . char

assign :: Parser Cmd
assign = do
  varName <- strExpr
  char '='
  value <- expr
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


tableLog = [[unary '!' Not], [binary '&' And], [binary '|' Or]]

binary sym f = Infix (symParser sym f) AssocLeft

binary' str f = Infix (strParser str f) AssocLeft
  where strParser s f = do
          string s
          spaces
          return f
        
unary sym f = Prefix (symParser sym f)

symParser s f = do
  char s
  spaces
  return f

tableArith = [[binary' "**" Exp], [binary '*' Mul, binary '/' Div, binary '%' Mod], 
              [binary '+' Plus, binary '-' Minus], [unary '-' Neg]]

prd :: Parser Pred
prd = buildExpressionParser tableLog other
  where other = cmp <|> parenPred
        cmp = do
          c <- comp
          spaces
          return (Pred c)
        parenPred = do
          symbol '('
          pr <- prd
          symbol ')'
          spaces
          return (Parens pr)

arith :: Parser ArithExpr
arith = buildExpressionParser tableArith other
  where other = (Val <$> expr) <|> parensArith
        parensArith = do
          symbol '('
          exp <- arith
          symbol ')'
          return (ParensAE exp)

block :: Parser [TLExpr]
block = do
  symbol '{'
  cmds <- many tlexpr
  symbol '}'
  return (cmds)

while :: Parser WhileLoop
while = do
  string "while"
  spaces
  symbol '('
  cnd <- prd
  spaces
  symbol ')'
  cmds <- block
  spaces
  return $ WhileLoop cnd cmds


conditional :: Parser Conditional
conditional = do
  string "if"
  spaces
  char '('
  cnd <- prd
  spaces
  char ')'
  spaces
  cmds1 <- block
  spaces
  el <- optionMaybe $ string "else"
  spaces
  cmds2 <- optionMaybe block
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
tlexpr = many comment *> choice [try (TLLoop <$> while), try (TLCmd <$> cmdOrAssign), (TLCnd <$> conditional)]
  
  


          
         
 






        
          
     
