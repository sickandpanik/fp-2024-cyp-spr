module Main where 

import Text.Printf (printf)
import Control.Monad (unless)

data Expr = Const Double | Operation Op

data Op = Sqrt Expr | BinOp BinOperator Expr Expr

data BinOperator = Exp | Mul | Div | Add | Sub
  deriving Eq

instance Show Expr where 
  show (Const x) = show x
  show (Operation op) = case op of
    (Sqrt e) -> "√(" ++ (show e) ++ ")"
    (BinOp op e1 e2) -> "(" ++ (show e1) ++ ") " ++ (show op) ++ " (" ++ (show e2) ++ ")"

instance Show BinOperator where
  show op = case op of
    Exp -> "^"
    Mul -> "*"
    Div -> "/"
    Add -> "+"
    Sub -> "-"

instance Eq Expr where 
  (==) (Const x1) (Const x2) = x1 == x2
  (==) (Operation (Sqrt e1)) (Operation (Sqrt e2)) = e1 == e2
  (==) (Operation (BinOp op1 lhs1 rhs1)) (Operation (BinOp op2 lhs2 rhs2)) = 
    (op1 == op2) && (lhs1 == lhs2) && (rhs1 == rhs2)
  (==) _ _ = False

data Error = Error ErrorKind Expr

data ErrorKind = DivisionByZero | NegativeNumberSquareRoot
  deriving Show

instance Show Error where 
  show (Error kind expr) = "ERROR: " ++ (show kind) ++ " in " ++ (show expr)

instance Eq Error where 
  (==) = undefined 

eval :: Expr -> Either Error Double
eval (Const x) = Right x
eval outerExpr@(Operation (Sqrt e)) = case e' of
  error@(Left _) -> error
  (Right x) -> if (x >= 0) then (Right (sqrt x)) else (Left (Error NegativeNumberSquareRoot outerExpr))
  where e' = eval e
eval outerExpr@(Operation (BinOp kind e1 e2)) = case (kind, e1', e2') of
  (_, error@(Left _), _) -> error
  (_, _, error@(Left _)) -> error
  (Div, (Right x), (Right y)) -> if (y /= 0) then (Right (x / y)) else (Left (Error DivisionByZero outerExpr))
  (_, (Right x), (Right y)) -> Right ((binOperatorToFunction kind) x y)
  where e1' = eval e1
        e2' = eval e2

binOperatorToFunction binOp = case binOp of
  Exp -> (**)
  Mul -> (*)
  Div -> (/)
  Add -> (+)
  Sub -> (-)

cases :: [(Expr, Either Error Double)]
cases = undefined 

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
  