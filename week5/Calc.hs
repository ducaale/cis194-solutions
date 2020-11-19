{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import ExprT
import Parser
import qualified StackVM as S

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add expr1 expr2) = (eval expr1) + (eval expr2)
eval (Mul expr1 expr2) = (eval expr1) * (eval expr2)

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

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

instance Expr S.Program where
  lit n = [S.PushI n]
  add expr1 expr2 = expr1 ++ expr2 ++ [S.Add] 
  mul expr1 expr2 = expr1 ++ expr2 ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul
 
