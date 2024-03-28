module Lib (eval, simplify, containsVariables) where

import Prelude hiding (lookup)
import Data.Map.Strict (Map, lookup)

import StateDemo
import Expr
import Error

eval :: (Ord a, Floating a) => Expr a -> State (Map String a) (Either (Error a) a)
eval expr = do
  state <- get
  return $ case expr of
    (Const x) -> Right x
    var@(Var varName) -> case lookup varName state of
      Nothing -> Left (Error UnknownVariable var)
      Just value -> Right value
    outerExpr@(Operation (Sqrt e)) -> case execState (eval e) state of
        Right x -> if x >= 0 then Right (sqrt x) else Left (Error NegativeNumberSquareRoot outerExpr)
        error -> error
    outerExpr@(Operation (BinOp kind e1 e2)) -> case (kind, execState (eval e1) state, execState (eval e2) state) of
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
