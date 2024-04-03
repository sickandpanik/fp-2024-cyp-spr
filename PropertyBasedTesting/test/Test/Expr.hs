module Test.Expr where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import Expr.AST
import qualified Expr.Infix as Infix
import qualified Expr.Prefix as Prefix

-- Here we check that parsers for arythmetics are correct. 
-- It's non-trivial how to generate strings which are likely to be parsed as expressions. 
-- Instead, we generate expressions which are printed into strings, and then parsed. 
-- This is not an ideal solution, because the printer works in one particular way, while 
-- users can do whatever they want. 
-- However this is better than nothing. 

-- For the simplest types, the generator just picks a random value from the list. 
genOp :: Gen Op
genOp = Gen.element [Plus, Minus, Mult, Div, Pow]

-- To generate a recursive algebraic data type, use Gen.recursive and Gen.subterm
genExpr :: Int -> Gen Expr
genExpr n =
  Gen.recursive
    Gen.choice
    [ -- non-recursive generators
      numGen
    -- , varGen
    ]
    [ -- recursive generators
      binOpGen
    -- , unOpGen 
    ]
  where
    numGen = Number <$> Gen.int (Range.constant 0 n)
    binOpGen = do
      op <- genOp
      Gen.subterm2 (genExpr n) (genExpr n) (BinOp op)

-- parser . printer == id
parserPrinterIsId :: MonadTest m => (Expr -> String) -> (String -> Maybe (String, Expr)) -> Expr -> m ()
parserPrinterIsId printer parser ast =
  case parser (printer ast) of
    Just ("", r) -> r === ast
    _ -> failure

prop_printerParserInfix :: Property
prop_printerParserInfix = property $ do
  expr <- forAll $ genExpr 100
  parserPrinterIsId printInfix Infix.parse expr

prop_printerParserPrefix :: Property
prop_printerParserPrefix = property $ do
  expr <- forAll $ genExpr 100
  parserPrinterIsId printPrefix Prefix.parse expr

props :: [TestTree]
props =
  [ testProperty "`parser . printer == id` for Infix" prop_printerParserInfix
  , testProperty "`parser . printer == id` for Prefix" prop_printerParserPrefix
  ]
