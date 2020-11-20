{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import qualified Data.Map as M

import ExprT
import Parser
import StackVM

eval :: ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Add expr1 expr2) = (eval expr1) + (eval expr2)
eval (ExprT.Mul expr1 expr2) = (eval expr1) * (eval expr2)

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax n1) (MinMax n2) = MinMax $ max n1 n2
  mul (MinMax n1) (MinMax n2) = MinMax $ min n1 n2

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 n1) (Mod7 n2) = Mod7 $ (n1 + n2) `mod` 7
  mul (Mod7 n1) (Mod7 n2) = Mod7 $ (n1 * n2) `mod` 7

instance Expr Program where
  lit n = [StackVM.PushI n]
  add expr1 expr2 = expr1 ++ expr2 ++ [StackVM.Add] 
  mul expr1 expr2 = expr1 ++ expr2 ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul
 
class HasVars a where
  var :: String -> a

data VarExprT
  = Lit Integer
  | Add VarExprT VarExprT
  | Mul VarExprT VarExprT
  | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = Main.Lit
  add = Main.Add
  mul = Main.Mul

instance HasVars VarExprT where
  var = Main.Var  

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit n _ = Just n
  add exp1 exp2 vs = do
    n1 <- (exp1 vs)
    n2 <- (exp2 vs)
    Just $ n1 + n2
  mul exp1 exp2 vs = do
    n1 <- (exp1 vs)
    n2 <- (exp2 vs)
    Just $ n1 * n2

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

