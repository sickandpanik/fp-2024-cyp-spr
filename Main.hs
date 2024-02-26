module Main where 

import Text.Printf (printf)
import Control.Monad (unless)

data Expr = Const Double | Operation Op
  deriving Eq

data Op = Sqrt Expr | BinOp BinOperator Expr Expr
  deriving Eq

data BinOperator = Exp | Mul | Div | Add | Sub
  deriving Eq

instance Show Expr where 
  show (Const x) = show x
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

data Error = Error ErrorKind Expr
  deriving Eq

data ErrorKind = DivisionByZero | NegativeNumberSquareRoot | RealPowerOfNegativeNumber
  deriving (Eq, Show)

instance Show Error where 
  show (Error kind expr) = printf "ERROR: %s in %s" (show kind) (show expr)

eval :: Expr -> Either Error Double
eval (Const x) = Right x
eval outerExpr@(Operation (Sqrt e)) = case eval e of
  error@(Left _) -> error
  Right x -> if x >= 0 then Right (sqrt x) else Left (Error NegativeNumberSquareRoot outerExpr)
eval outerExpr@(Operation (BinOp kind e1 e2)) = case (kind, eval e1, eval e2) of
  (_, error@(Left _), _) -> error
  (_, _, error@(Left _)) -> error
  (Div, Right x, Right y) | y == 0 -> Left (Error DivisionByZero outerExpr)
  (Exp, Right x, Right y) | x < 0 -> Left (Error RealPowerOfNegativeNumber outerExpr)
  (_, Right x, Right y) -> Right (binOperatorToFunction kind x y)

binOperatorToFunction binOp = case binOp of
  Exp -> (**)
  Mul -> (*)
  Div -> (/)
  Add -> (+)
  Sub -> (-)

cases :: [(Expr, Either Error Double)]
cases = 
  [ let expr = (Const 2) in (expr, Right 2), -- 2
    let expr = Operation (BinOp Div (Const 2) (Const 0)) in -- 2 / 0
      (expr, Left (Error DivisionByZero expr)),
    let expr = Operation (Sqrt (Const (-1))) in -- √(-1)
      (expr, Left (Error NegativeNumberSquareRoot expr)),
    let expr = Operation (BinOp Exp (Const (-1)) (Const 0.5)) in -- (-1)^(1/2)
      (expr, Left (Error RealPowerOfNegativeNumber expr)),
    (Operation (BinOp Add (Const 2) (Operation (BinOp Mul (Const 2) (Const 2)))), Right 6.0), -- 2 + 2 * 2
    (Operation (BinOp Exp (Const 2) (Operation (BinOp Div (Const 1) (Const 2)))), Right (sqrt 2.0)), -- 2^(1/2)
    (Operation (BinOp Exp (Const 5) (Const (-1))), Right 0.2), -- 5^(-1)
    let expr = (Operation (BinOp Div (Const 16) (Operation (BinOp Sub (Operation (BinOp Mul (Const 2) (Const 50000))) (Operation (BinOp Mul (Const 1000) (Const 100))))))) in -- 16 / (2 * 50000 - 1000 * 100)
      (expr, Left (Error DivisionByZero expr)),
    (Operation (BinOp Sub (Const 15) (Operation (BinOp Div (Const 64) (Const 4)))), Right (-1)), -- 15 - (64 / 4)
    (Operation (BinOp Div (Const (-21)) (Const 3)), Right (-7)) -- -21 / 3
  ]


test :: Expr -> Either Error Double -> IO () 
test expr expected = 
    let actual = eval expr in 
    unless (expected == actual) $ describeFailure actual
  where 
    describeFailure actual = 
      printf "eval (%s) should be %s but it was %s" (show expr) (show expected) (show actual) 
  

main :: IO () 
main = do 
  mapM_ (uncurry test) cases 
  