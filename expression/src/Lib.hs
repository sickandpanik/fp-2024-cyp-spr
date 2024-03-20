module Lib (eval, simplify) where

import Prelude hiding (lookup)
import Data.Map.Strict (Map, lookup)

import Expr
import Error

eval :: (Ord a, Floating a) => Expr a -> Map String a -> Either (Error a) a
eval (Const x) _ = Right x
eval var@(Var varName) vars = case lookup varName vars of
  Nothing -> Left (Error UnknownVariable var)
  Just value -> Right value
eval outerExpr@(Operation (Sqrt e)) vars = case eval e vars of
  error@(Left _) -> error
  Right x -> if x >= 0 then Right (sqrt x) else Left (Error NegativeNumberSquareRoot outerExpr)
eval outerExpr@(Operation (BinOp kind e1 e2)) vars = case (kind, eval e1 vars, eval e2 vars) of
  (_, error@(Left _), _) -> error
  (_, _, error@(Left _)) -> error
  (Div, _, Right y) | y == 0 -> Left (Error DivisionByZero outerExpr)
  (Exp, Right x, _) | x < 0 -> Left (Error RealPowerOfNegativeNumber outerExpr)
  (_, Right x, Right y) -> Right (binOperatorToFunction kind x y)

simplify :: (Eq a, Num a) => Expr a -> Expr a
simplify (Operation (BinOp Mul _ (Const 0))) = Const 0
simplify (Operation (BinOp Mul (Const 0) _)) = Const 0
simplify (Operation (BinOp Add (Const 0) rhs)) = rhs
simplify (Operation (BinOp Add lhs (Const 0))) = lhs

binOperatorToFunction binOp = case binOp of
  Exp -> (**)
  Mul -> (*)
  Div -> (/)
  Add -> (+)
  Sub -> (-)
