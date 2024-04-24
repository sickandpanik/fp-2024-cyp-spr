import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec (parse)

import LTerm
import LTermParser

main :: IO ()
main = defaultMain tests

tests = testGroup "Tests" [parseTests, showTests]

data TestLTerm = TestLTerm { 
  canonicRep :: String,
  stringRep :: String,
  haskellRep :: LTerm
}

testLTerms :: [TestLTerm]
testLTerms = [
    TestLTerm "\\xyz.xyz" 
      "\\x y z.x y z" (Fun "x" (Fun "y" (Fun "z" (Apply (Apply (Var "x") (Var "y")) (Var "z")))))
  , TestLTerm "(\\x.xx) (\\y.yy)"
      "(\\x.x x) (\\y.y y)" (Apply (Fun "x" (Apply (Var "x") (Var "x"))) (Fun "y" (Apply (Var "y") (Var "y"))))
  , TestLTerm "\\zw.(\\y.(\\x.xz)y)w"
      "\\z w.(\\y.(\\x.x z) y) w" (Fun "z" (Fun "w" (Apply (Fun "y" (Apply (Fun "x" (Apply (Var "x") (Var "z"))) (Var "y"))) (Var "w"))))
  , TestLTerm "\\y.(\\x.xz)y"
      "\\y.(\\x.x z) y" (Fun "y" (Apply (Fun "x" (Apply (Var "x") (Var "z"))) (Var "y")))
  , TestLTerm "fg(\\x.x(xx)x)"
      "f g (\\x.x (x x) x)" (Apply (Apply (Var "f") (Var "g")) (Fun "x" (Apply (Apply (Var "x") (Apply (Var "x") (Var "x"))) (Var "x"))))
  , TestLTerm "S-combinator [\\fgx.fx(gx)]"
      "\\f g x.f x (g x)" (Fun "f" (Fun "g" (Fun "x" (Apply (Apply (Var "f") (Var "x")) (Apply (Var "g") (Var "x"))))))
  ]  

-- ## parse tests

testLTermToParseTest :: TestLTerm -> TestTree
testLTermToParseTest t = testCase (canonicRep t) $ parse parseLTerm "" (stringRep t) @?= Right (haskellRep t)

parseTests = testGroup "parse" $ map testLTermToParseTest testLTerms

-- ## show tests

testLTermToShowTest :: TestLTerm -> TestTree
testLTermToShowTest t = testCase (canonicRep t) $ show (haskellRep t) @?= stringRep t

showTests = testGroup "show" $ map testLTermToShowTest testLTerms
