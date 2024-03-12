module Lib
    ( Expr, eval, simplify, test, cases
    ) where

import Prelude hiding (lookup)
import Text.Printf (printf)
import Control.Monad (unless)
import Data.Map.Strict (Map, empty, lookup, fromList)

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

data Error a = Error ErrorKind (Expr a)
  deriving Eq

data ErrorKind = DivisionByZero | NegativeNumberSquareRoot | RealPowerOfNegativeNumber | UnknownVariable
  deriving (Eq, Show)

instance (Show a) => Show (Error a) where
  show (Error kind expr) = printf "ERROR: %s in %s" (show kind) (show expr)

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

data TestCase = TestCase {
  expr :: Expr Double,
  vars :: Map String Double,
  expect :: Either (Error Double) Double
}

constructCase :: (Expr Double, Map String Double, Either (Error Double) Double) -> TestCase
constructCase (expr, vars, exp) = TestCase expr vars exp

constructCaseNoTest :: (Expr Double, Either (Error Double) Double) -> TestCase
constructCaseNoTest (expr, exp) = TestCase expr empty exp

cases :: [TestCase]
cases =
  map constructCaseNoTest [
    let expr = Const 2 in (expr, Right 2) -- 2
  , let expr = Operation (BinOp Div (Const 2) (Const 0)) in -- 2 / 0
      (expr, Left (Error DivisionByZero expr))
  , let expr = Operation (Sqrt (Const (-1))) in -- √(-1)
     (expr, Left (Error NegativeNumberSquareRoot expr))
  , let expr = Operation (BinOp Exp (Const (-1)) (Const 0.5)) in -- (-1)^(1/2)
      (expr, Left (Error RealPowerOfNegativeNumber expr))
  , (Operation (BinOp Add (Const 2) (Operation (BinOp Mul (Const 2) (Const 2)))), Right 6.0) -- 2 + 2 * 2
  , (Operation (BinOp Exp (Const 2) (Operation (BinOp Div (Const 1) (Const 2)))), Right (sqrt 2.0)) -- 2^(1/2)
  , (Operation (BinOp Exp (Const 5) (Const (-1))), Right 0.2) -- 5^(-1)
  , let expr = Operation (BinOp Div (Const 16) (Operation (BinOp Sub (Operation (BinOp Mul (Const 2) (Const 50000))) (Operation (BinOp Mul (Const 1000) (Const 100)))))) in -- 16 / (2 * 50000 - 1000 * 100)
      (expr, Left (Error DivisionByZero expr))
  , (Operation (BinOp Sub (Const 15) (Operation (BinOp Div (Const 64) (Const 4)))), Right (-1)) -- 15 - (64 / 4)
  , (Operation (BinOp Div (Const (-21)) (Const 3)), Right (-7)) -- -21 / 3
  ] 
  ++ 
  map constructCase [
    (Var "b", empty, Left (Error UnknownVariable (Var "b")))
  ]


test :: TestCase -> IO ()
test case' =
    let actual = eval (expr case') (vars case') in
    unless (expect case' == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "eval (%s)\n with context %s\n should be  %s\n but it was %s\n" (show (expr case')) (show (vars case')) (show (expect case')) (show actual)
