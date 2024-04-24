import Test.Tasty
import Test.Tasty.HUnit

import LTerm

main :: IO ()
main = defaultMain tests

tests = testGroup "Tests" [showTests]

-- ## show tests

showTests = testGroup "show" $ map (uncurry testCase) [
    ("Show \\xyz.xyz", show (Fun "x" (Fun "y" (Fun "z" (Apply (Apply (Var "x") (Var "y")) (Var "z"))))) @?= 
        "\\x y z.x y z")
  , ("Show (\\x.xx) (\\y.yy)", show (Apply (Fun "x" (Apply (Var "x") (Var "x"))) (Fun "y" (Apply (Var "y") (Var "y")))) @?= 
        "(\\x.x x) (\\y.y y)")
  , ("Show \\zw.(\\y.(\\x.xz)y)w", show (Fun "z" (Fun "w" (Apply (Fun "y" (Apply (Fun "x" (Apply (Var "x") (Var "z"))) (Var "y"))) (Var "w")))) @?= 
        "\\z w.(\\y.(\\x.x z) y) w")
  , ("Show \\y.(\\x.xz)y", show (Fun "y" (Apply (Fun "x" (Apply (Var "x") (Var "z"))) (Var "y"))) @?= 
        "\\y.(\\x.x z) y")
  , ("Show fg(\\x.x(xx)x)", show (Apply (Apply (Var "f") (Var "g")) (Fun "x" (Apply (Apply (Var "x") (Apply (Var "x") (Var "x"))) (Var "x")))) @?=
        "f g (\\x.x (x x) x)")
  , ("Show S-combinator [\\fgx.fx(gx)]", show (Fun "f" (Fun "g" (Fun "x" (Apply (Apply (Var "f") (Var "x")) (Apply (Var "g") (Var "x")))))) @?=
        "\\f g x.f x (g x)")
  ]
