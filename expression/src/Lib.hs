module Lib (eval, simplify, containsVariables) where

import Prelude hiding (lookup)
import Data.Map.Strict (Map, lookup)

import StateDemo
import Expr
import Error

eval :: (Ord a, Floating a) => Expr a -> State (Map String a) (Either (Error a) a)
eval (Const x) = return $ Right x
eval var@(Var varName) = do
  state <- get
  return $ case lookup varName state of
    Nothing -> Left (Error UnknownVariable var)
    Just value -> Right value
eval outerExpr@(Operation (Sqrt e)) = do
  result <- eval e
  return $ case result of
    Right x -> if x >= 0 then Right (sqrt x) else Left (Error NegativeNumberSquareRoot outerExpr)
    error -> error
eval outerExpr@(Operation (BinOp kind e1 e2)) = do
  result1 <- eval e1
  result2 <- eval e2
  return $ case (kind, result1, result2) of
    (Div, _, Right y) | y == 0 -> Left (Error DivisionByZero outerExpr)
    (Exp, Right x, _) | x < 0 -> Left (Error RealPowerOfNegativeNumber outerExpr)
    (_, Right x, Right y) -> Right (binOperatorToFunction kind x y)
    (_, error, _) -> error

binOperatorToFunction binOp = case binOp of
  Exp -> (**)
  Mul -> (*)
  Div -> (/)
  Add -> (+)
  Sub -> (-)

simplify :: (Eq a, Num a) => Expr a -> Expr a
simplify (Operation (BinOp Mul _ (Const 0))) = Const 0
simplify (Operation (BinOp Mul (Const 0) _)) = Const 0
simplify (Operation (BinOp Add (Const 0) rhs)) = rhs
simplify (Operation (BinOp Add lhs (Const 0))) = lhs

containsVariables :: Expr a -> Bool
containsVariables expr = case expr of
  Var _ -> True
  Operation (Sqrt e) -> containsVariables e
  Operation (BinOp _ e1 e2) -> containsVariables e1 || containsVariables e2
  _ -> False
