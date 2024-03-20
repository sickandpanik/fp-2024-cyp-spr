module Error where

import Text.Printf (printf)

import Expr

data Error a = Error ErrorKind (Expr a)
  deriving Eq

data ErrorKind = DivisionByZero | NegativeNumberSquareRoot | RealPowerOfNegativeNumber | UnknownVariable
  deriving (Eq, Show)

instance (Show a) => Show (Error a) where
  show (Error kind expr) = printf "ERROR: %s in %s" (show kind) (show expr)
