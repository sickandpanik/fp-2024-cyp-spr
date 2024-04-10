import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf (printf)
import Prelude hiding (lookup)
import Data.Map.Strict (Map, empty, lookup, fromList)

import StateDemo
import Expr
import Error
import Lib
import Parser
import ExprParser
import ParsingError

main :: IO ()
main = defaultMain tests

tests = testGroup "Tests" [evalTests, parserTests]


-- ## eval tests

evalTests = testGroup "eval" $
    map convertCaseToHUnit evalCases

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
convertCaseToHUnit (ExprTestCase expr vars expect) = testCase (printf "%s @?= %s" (show expr) (show expect)) (execState (eval expr) vars @?= expect)

evalCases :: [ExprTestCase]
evalCases =
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
  , (Operation (BinOp Add (Var "a") (Var "bcd")), fromList [("a", 33), ("bcd", -16)], Right (17))
  ]


-- ## parsers tests

parserTests = testGroup "parser" [
    parseConstCases
  , parseIdentCases
  , parseVarCases
  , parseUnaryOpCases
  , parseBinOpCases
  , parseExprCases
  ]

parseConstCases = testGroup "parseConst" $ map (uncurry testCase) [
    ("Parse const '35'", runParser parseConst "35" @?= Right ("", Const 35))
  , ("Parse const, leave out the suffix", runParser parseConst "563aaa" @?= Right ("aaa", Const 563))
  , ("Parse const with leading zeroes", runParser parseConst "0000555" @?= Right ("", Const 555))
  , ("Discard decimal dot", runParser parseConst "5.666" @?= Right (".666", Const 5))
  , ("Parse const '0'", runParser parseConst "0" @?= Right ("", Const 0))
  , ("Break when string doesn't start with digit", runParser parseConst "aa0" @?= Left Unexpected)
  ]

parseIdentCases = testGroup "parseIdent" $ map (uncurry testCase) [
    ("Parse identifier 'sqrt'", runParser parseIdent "sqrt" @?= Right ("", "sqrt"))
  , ("Parse identifier 'X_Æ_A12'", runParser parseIdent "X_Æ_A12" @?= Right ("", "X_Æ_A12"))
  , ("Break when string starts with digit", runParser parseIdent "33aa" @?= Left Unexpected)
  , ("Discard everything after space", runParser parseIdent "foo bar" @?= Right (" bar", "foo"))
  ]

parseVarCases = testGroup "parseVar" $ map (uncurry testCase) [
    ("Parse variable 'x'", runParser parseVar "x" @?= Right ("", Var "x"))
  , ("Break when encountering 'sqrt' keyword", runParser parseVar "sqrt" @?= Left Unexpected)
  ]

parseUnaryOpCases = testGroup "parseUnaryOp" $ map (uncurry testCase) [
    ("Parse 'sqrt '", runParser parseUnaryOp "sqrt " @?= Right (" ", "sqrt"))
  , ("Break when identifier is not valid unary op", runParser parseUnaryOp "aabb" @?= Left Unexpected)
  ]

parseBinOpCases = testGroup "parseBinOperator" $ map (uncurry testCase) [
    ("Parse '+'", runParser parseBinOperator "+" @?= Right ("", Add))
  , ("Parse '/ '", runParser parseBinOperator "/ " @?= Right (" ", Div))
  , ("Break on every other character", runParser parseBinOperator "::::::" @?= Left Unexpected)
  , ("Break on empty string", runParser parseBinOperator "" @?= Left Unexpected)
  ]

parseExprCases = testGroup "parseExpr" $ map (uncurry testCase) [
    ("Parse '+ 2 2'", runParser parseExpr "+ 2 2" @?= Right ("", Operation $ BinOp Add (Const 2) (Const 2)))
  , ("Break on '- 123'", runParser parseExpr "- 123" @?= Left Unexpected)
  , ("Break on 'sqrt '", runParser parseExpr "sqrt " @?= Left Unexpected)
  , ("Parse nested: '/ 16 - * 2 50000 * 10 10000'", runParser parseExpr "/ 16 - * 2 50000 * 10 10000" @?= Right ("", Operation $ BinOp Div (Const 16) (Operation $ BinOp Sub (Operation $ BinOp Mul (Const 2) (Const 50000)) (Operation $ BinOp Mul (Const 10) (Const 10000)))))
  , ("Parse '* rho * g h'", runParser parseExpr "* rho * g h" @?= Right ("", Operation $ BinOp Mul (Var "rho") (Operation $ BinOp Mul (Var "g") (Var "h"))))
  , ("Parse '* 4 * pi ^ r 2 + 2 2', leave out ' + 2 2' part", runParser parseExpr "* 4 * pi ^ r 2 + 2 2" @?= Right (" + 2 2", Operation $ BinOp Mul (Const 4) (Operation $ BinOp Mul (Var "pi") (Operation $ BinOp Exp (Var "r") (Const 2)))))
  ]