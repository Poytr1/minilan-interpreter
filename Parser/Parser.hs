module Parser.Parser
  (getAST, parseProgram, parseExpr)
where

import Data.DataType
import Parser.Lexer
import Parser.Rename

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Pos

import Control.Monad.Except

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok


parseVar :: Parser LitVar
parseVar = do
  pos <- getPosition
  name <- funPara <|> identifier
  return (Var name pos)

funPara :: Parser String
funPara = do
  reserved "Funname"
  name <- identifier
  return name

funExpr :: Parser LitExpr
funExpr = do
  pos <- getPosition
  name <- funPara
  return (Id (Var name pos))

end = reserved "End"
endWith n = end  >> n
endReturn n = endWith (return n)

num :: Parser LitExpr
num = do
  --reserved "Num"
  n <- integer
  return (Num n)

var :: Parser LitExpr
var = do
  --reserved "Id"
  name <- parseVar
  return (Id name)

appFun :: Parser LitExpr
appFun = do
  reserved "Apply"
  name <- parseVar
  reserved "Argus"
  args  <- argList
  endReturn (Apply name args)

negb :: Parser LitExpr
negb = do
  reserved "Negb"
  e <- expr
  return (Negb e)

arithmatic :: String ->  (LitExpr -> LitExpr -> LitExpr)
   -> Parser LitExpr
arithmatic name cons = do
  reserved name
  e1 <- expr
  e2 <- expr
  return (cons e1 e2)

arithmatics = foldr1 (<|>) ops
  where ops = zipWith arithmatic ["Plus", "Minus", "Mult" , "Lt", "Gt", "And", "Or", "Mod", "Div"]
                                        [Plus, Minus, Mult, Lt, Gt, And, Or, Mod, Div]

argList :: Parser (ArgList LitVar)
argList = do
  --reserved "List"
  lis <- many (expr <|> funExpr)
  return lis

expr :: Parser LitExpr
expr = num
  <|> var
  <|> appFun
  <|> arithmatics
  <|> negb

parseExpr :: String -> Either ParseError (Expr LitVar)
parseExpr  = parse expr "minilanInterpreter"

decl :: Parser LitCommand
decl = do
  reserved "Var"
  vars <- varList
  endReturn (Decl vars)

value :: Parser LitCommand
value = fmap Value expr

func :: Parser LitCommand
func = do
  reserved "Function"
  name <- parseVar
  paras <- paraList
  pros <- program
  return (Func name paras pros)

letBe :: Parser LitCommand
letBe = do
  reserved "Assign"
  var <- parseVar
  ex <- expr
  return (Assign var ex)

ifcommand :: Parser LitCommand 
ifcommand = do 
  reserved "If"
  ex <- expr
  b1 <- program
  reserved "Else"
  b2 <- program
  return (If ex b1 b2)

whileCommand :: Parser LitCommand 
whileCommand = do
  reserved "While"
  ex <- expr
  blk <- program
  return (While ex blk)

runFun :: Parser LitCommand
runFun = do
  reserved "Call"
  name <- parseVar
  reserved "Argus"
  args <- argList
  endReturn (Call name args)

simple :: (String, LitExpr -> LitCommand)
   -> Parser LitCommand
simple (name, f) = do
  reserved name
  ex <- expr
  return (f ex)

returnPrint = foldr1 (<|>) (map simple
                                                [("Return", Return),
                                                 ("Print", Print)]  )

rea :: Parser LitCommand
rea  = do
  reserved "Read"
  name <- parseVar
  return (Read name)


command :: Parser LitCommand
command = foldr1 (<|>)
  [decl, value, func, letBe, runFun, returnPrint, rea, ifcommand, whileCommand]

program :: Parser LitProgram
program = do
  reserved "Begin"
  cs <- many command
  endReturn cs


paraList :: Parser (ParaList LitVar)
paraList = do
  reserved "Paras"
  vars <- varList
  return vars

varList :: Parser [LitVar]
varList = many parseVar

toStage :: Either ParseError a -> Stage a
toStage (Right a) = return a
toStage (Left err) = throwError (Parser err)

parseProgram :: String -> String -> Either ParseError LitProgram
parseProgram  = parse program

getAST :: String -> String -> Stage RenamedProgram
getAST name = (>>=  runRename) . toStage . parseProgram name
