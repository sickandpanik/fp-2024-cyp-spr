module Expr where

import Text.Printf (printf)

data Expr a = Const a | Var String | Operation (Op a)
  deriving Eq

data Op a = Sqrt (Expr a) | BinOp BinOperator (Expr a) (Expr a)
  deriving Eq

data BinOperator = Exp | Mul | Div | Add | Sub
  deriving Eq

instance (Num a) => Num (Expr a) where
    (+) lhs rhs = Operation (BinOp Add lhs rhs)
    (*) lhs rhs = Operation (BinOp Mul lhs rhs)
    (-) lhs rhs = Operation (BinOp Sub lhs rhs)
    fromInteger int = Const (fromInteger int)

instance (Show a) => Show (Expr a) where
  show (Const x) = show x
  show (Var varName) = varName
  show (Operation op) = case op of
    (Sqrt e) -> printf "√(%s)" (show e)
    (BinOp op e1 e2) -> printf "(%s) %s (%s)" (show e1) (show op) (show e2)

instance Show BinOperator where
  show op = case op of
    Exp -> "^"
    Mul -> "*"
    Div -> "/"
    Add -> "+"
    Sub -> "-"
