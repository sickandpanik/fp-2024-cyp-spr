module ParsingError where

data ParsingError = Unexpected | EmptyLanguage
  deriving (Eq, Show)