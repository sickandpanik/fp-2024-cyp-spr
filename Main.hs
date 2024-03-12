module Main where

import Text.Printf (printf)
import Control.Monad (unless)

-- Instances of Functor

{- 1. Either Functor

   Since errors are usually constructed with the Left data constructor,
   it does not make sense to modify them, if one has occured. However,
   if we have have Right with some value, we could modify it.

   We only need to check that identity (fmap id == id) holds. Indeed, in
   case of error it holds automatically, as we return the same error; in 
   case of some value in the Right, we apply id to it and get the same
   value.
-}

newtype Either' a b = Either' (Either a b)

instance Functor (Either' errorType) where
  fmap :: (a -> b) -> Either' errorType a -> Either' errorType b
  fmap f (Either' (Right value)) = Either' (Right (f value))
  fmap f (Either' (Left error)) = Either' (Left error) -- how do I simplify this?

{- 2. Arrow type functor

   In case of a function, it makes sense to apply the fmap function
   after the original function has been applied. Thus the original 
   function behaves as expected; we just transform the result somehow.

   The identity holds because identity is a neutral element in relation
   to functions a -> b.
-}

newtype Arrow a b = Arrow (a -> b)

instance Functor (Arrow a) where
  fmap :: (b -> c) -> Arrow a b -> Arrow a c
  fmap f (Arrow g) = Arrow (f . g)

---

data Expr a = Const a | Var String | Operation (Op a)
  deriving Eq

data Op a = Sqrt (Expr a) | BinOp BinOperator (Expr a) (Expr a)
  deriving Eq

data BinOperator = Exp | Mul | Div | Add | Sub
  deriving Eq

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

data ErrorKind = DivisionByZero | NegativeNumberSquareRoot | RealPowerOfNegativeNumber | UnknownVariable | AmbiguousBindings
  deriving (Eq, Show)

instance (Show a) => Show (Error a) where
  show (Error kind expr) = printf "ERROR: %s in %s" (show kind) (show expr)

eval :: (Ord a, Floating a) => Expr a -> [(String, a)] -> Either (Error a) a
eval (Const x) _ = Right x
eval var@(Var varName) vars = case filter ((== varName) . fst) vars of -- I couldn't use find here for some reason
  [] -> Left (Error UnknownVariable var)
  [h] -> Right (snd h)
  _ -> Left (Error AmbiguousBindings var)
eval outerExpr@(Operation (Sqrt e)) vars = case eval e vars of
  error@(Left _) -> error
  Right x -> if x >= 0 then Right (sqrt x) else Left (Error NegativeNumberSquareRoot outerExpr)
eval outerExpr@(Operation (BinOp kind e1 e2)) vars = case (kind, eval e1 vars, eval e2 vars) of
  (_, error@(Left _), _) -> error
  (_, _, error@(Left _)) -> error
  (Div, _, Right y) | y == 0 -> Left (Error DivisionByZero outerExpr)
  (Exp, Right x, _) | x < 0 -> Left (Error RealPowerOfNegativeNumber outerExpr)
  (_, Right x, Right y) -> Right (binOperatorToFunction kind x y)

simplify :: (Ord a) => Expr a -> Expr a
simplify (Operation (BinOp Mul _ (Const 0))) = Const 0
simplify (Operation (BinOp Mul (Const 0) _)) = Const 0



binOperatorToFunction binOp = case binOp of
  Exp -> (**)
  Mul -> (*)
  Div -> (/)
  Add -> (+)
  Sub -> (-)

data TestCase = TestCase {
  expr :: Expr Double,
  vars :: [(String, Double)],
  expect :: Either (Error Double) Double
}

constructCase :: (Expr Double, [(String, Double)], Either (Error Double) Double) -> TestCase
constructCase (expr, vars, exp) = TestCase expr vars exp

constructCaseNoTest :: (Expr Double, Either (Error Double) Double) -> TestCase
constructCaseNoTest (expr, exp) = TestCase expr [] exp

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
    (Var "b", [], Left (Error UnknownVariable (Var "b")))
  , (Var "b", [("b", 5), ("b", -1)], Left (Error AmbiguousBindings (Var "b")))
  ]


test :: TestCase -> IO ()
test case' =
    let actual = eval (expr case') (vars case') in
    unless (expect case' == actual) $ describeFailure actual
  where
    describeFailure actual =
      printf "eval (%s)\n with context %s\n should be  %s\n but it was %s\n" (show (expr case')) (show (vars case')) (show (expect case')) (show actual)

main :: IO ()
main = do
  mapM_ test cases
