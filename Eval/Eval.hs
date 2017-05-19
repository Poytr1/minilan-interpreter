{-#LANGUAGE TemplateHaskell#-}
module Eval.Eval(runProgram) where

import Data.DataType

import Control.Lens
import qualified Data.IntMap.Strict as I
import Control.Monad.State
import Data.Maybe

data Env = Env { _varTable :: Record
  , _output :: [String]
  --, _returned :: Bool
  , _input :: [String]
}

makeLenses '' Env

nullEnv = Env I.empty [] --False

type EvalState = StateT Env Stage

type EvalResult = EvalState Result

evalVar :: IntVar -> EvalResult
evalVar (Slot n _ _) = uses varTable (fromJust . I.lookup n)

setVar :: IntVar -> Result -> EvalState ()
setVar (Slot n _ _) res = varTable %= I.insert n res

declareVar :: IntVar -> EvalState ()
declareVar v = do
  setVar v None
  printDeclare v

printDeclare v =  statePrint $ "Allocating memory for " ++ prettyShow v

printReclaim v = statePrint $ "Reclaiming the memory of " ++ prettyShow v

statePrint :: String -> EvalState ()
statePrint s = (lift . lift) (putStrLn s)

applyFun :: IntVar -> Result -> [Result] -> EvalResult
applyFun f (Lambda lis pro clos) argLis
  | length lis /= length argLis = throw FunError
  | otherwise = do
    statePrint ("Creating activation record for " ++ prettyShow f)
    mapM_ printDeclare lis
    let nlis = map unpackLit lis
    varTable %= I.union (I.fromList (zip nlis argLis))
    value <- evalProgram pro
    --statePrint $ "return value " ++ show value
    statePrint ("Garbage collection of " ++ prettyShow f)
    mapM_ printReclaim clos
    return value

evalExpr :: RenamedExpr -> EvalResult
evalExpr (Num n) = return $ Int n
evalExpr (Id var) = evalVar var
evalExpr (Apply var paras) = do
  fun <- evalVar var
  rs <- mapM evalExpr paras
  applyFun var fun rs
evalExpr (Plus e1 e2) = evalArith (+) e1 e2
evalExpr (Minus e1 e2) = evalArith (-) e1 e2
evalExpr (Mult e1 e2) = evalArith (*) e1 e2
evalExpr (Div e1 e2) = evalArith div e1 e2
evalExpr (Mod e1 e2) = evalArith mod e1 e2 
evalExpr (Eq el er) = do 
  e1 <- evalExpr el
  e2 <- evalExpr er
  case (e1, e2) of
    (Int n1, Int n2) -> do if n1 == n2 then do return $Int 1
                            else do return $Int 0
evalExpr (Lt el er) = do 
  e1 <- evalExpr el
  e2 <- evalExpr er
  case (e1, e2) of
    (Int n1, Int n2) -> do if n1 < n2 then do return $Int 1
                            else do return $Int 0
evalExpr (Gt el er) = do 
  e1 <- evalExpr el
  e2 <- evalExpr er
  case (e1, e2) of 
    (Int n1, Int n2) -> do if n1 > n2 then do return $Int 1
                            else do return $Int 0
evalExpr (And e1 e2) = do
  v1 <- evalExpr e1
  case v1 of 
    Int 0 -> do return $Int 0
    Int 1 -> do v2 <- evalExpr e2
                return v2 

evalExpr (Or e1 e2) = do 
  v1 <- evalExpr e1
  case v1 of 
    Int 1 -> do return $Int 1
    Int 0 -> do v2 <- evalExpr e2
                return v2

evalExpr (Negb e) = do
  v <- evalExpr e
  case v of
    Int 1 -> do return $Int 0
    Int 0 -> do return $Int 1
  
evalArith :: (Integer -> Integer -> Integer)
   -> RenamedExpr -> RenamedExpr
    -> EvalResult
evalArith f e1 e2 = do
  (Int n1) <- evalToInt e1
  (Int n2) <- evalToInt e2
  return (Int (f n1 n2))

evalToInt :: RenamedExpr -> EvalResult
evalToInt e = do
  ne <- evalExpr e
  case ne of
    Int _ -> return ne
    _ -> throw (ExpectedInt ne)

evalToLambda :: RenamedExpr -> EvalResult
evalToLambda e = do
  ne <- evalExpr e
  case ne of
    Lambda {}-> return ne
    _ -> throw (ExpectedInt ne)

noneRes = return None

evalCommand :: RenamedCommand -> EvalResult
evalCommand (Decl vs) = mapM_ declareVar vs >> noneRes
evalCommand (Value expr) = evalExpr expr
evalCommand (Record var pars pros clos) = setVar var (Lambda pars pros clos) >> noneRes
evalCommand (Assign v e) = do
  ne <- evalExpr e
  setVar v ne
  noneRes
evalCommand (Call f args) = evalExpr (Apply f args) >> noneRes
evalCommand (Return e) = do
    res <- evalExpr e
    --statePrint $ "Return !" ++ show res
    --returned .= True
    return res
evalCommand (Read e) = readBuffer >>= setVar e >> noneRes
evalCommand (Print expr) = do
  e <- evalExpr expr
  printBuffer e 
  noneRes
evalCommand (If e b1 b2) = do 
    res <- evalExpr e
    case res of
      Int 1 -> do ret <- evalProgram b1
                  case ret of
                    Int 0 -> do noneRes
                    otherwise -> do return ret
      Int 0 -> do ret <- evalProgram b2
                  case ret of
                    Int 0 -> do noneRes
                    otherwise -> do return ret
   -- case res of 
   --   Int 1 -> do ret <- evalProgram b1
    --  Int 0 -> do evalProgram b2
               --   ret <- use returned


evalCommand (While e b) = do
    res <- evalExpr e 
    case res of 
      Int 1 -> do evalProgram b
                  evalCommand (While e b)
      Int 0 -> do noneRes

readBuffer :: EvalResult
readBuffer = do
  buff <- use input
  case buff of
    [] -> throw EOF
    i:_ -> do
      input %= tail
      tryRead i

tryRead :: String -> EvalResult
tryRead s =
  case reads s of
    [(n, _)] -> return (Int n)
    _ -> throw ReadError

printBuffer :: Result -> EvalResult
printBuffer (Int n) =
  output %= (show n :) >> noneRes
printBuffer _ = throw PrintError

evalProgram :: RenamedProgram -> EvalResult
evalProgram [] = return (Int 0)
evalProgram (c:cs) = do
  res <- evalCommand c
  --ret <- use returned
  --statePrint $ "res value " ++ show res
  case res of
    None -> do evalProgram cs
    otherwise -> do --if ret then do
         --returned .= False
                  --statePrint $ "res value " ++ show res
                  return res
  --else evalProgram cs

runProgram :: RenamedProgram -> [String] -> Stage [String]
runProgram pro input =
  fmap (^. output) (execStateT (evalProgram pro) (nullEnv input))
