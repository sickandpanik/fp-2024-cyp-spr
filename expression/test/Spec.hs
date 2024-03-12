import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf (printf)
import Prelude hiding (lookup)
import Data.Map.Strict (Map, empty, lookup, fromList)

import Lib

main :: IO ()
main = defaultMain unitTests

unitTests = testGroup "Unit tests" $
    map convertCaseToHUnit cases

data ExprTestCase = ExprTestCase {
  expr :: Expr Double,
  vars :: Map String Double,
  expect :: Either (Error Double) Double
}

constructCase :: (Expr Double, Map String Double, Either (Error Double) Double) -> ExprTestCase
constructCase (expr, vars, exp) = ExprTestCase expr vars exp

constructCaseNoTest :: (Expr Double, Either (Error Double) Double) -> ExprTestCase
constructCaseNoTest (expr, exp) = ExprTestCase expr empty exp

convertCaseToHUnit :: ExprTestCase -> TestTree
convertCaseToHUnit (ExprTestCase expr vars expect) = testCase (printf "%s @?= %s" (show expr) (show expect)) (eval expr vars @?= expect)

cases :: [ExprTestCase]
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

