module LTermParser where

import LTerm
import Text.Parsec

parseVar :: Parsec String st LTerm
parseVar = do
  res <- many1 letter
  return (Var res)

parseFun :: Parsec String st LTerm
parseFun = do
  char '\\'
  skipMany space
  varsNames <- sepBy1 (many1 letter) (many1 space)
  skipMany space
  char '.'
  skipMany space
  inner <- parseLTerm
  return $ foldr Fun inner varsNames

parsePrimLTerm :: Parsec String st LTerm
parsePrimLTerm = choice [
    parseFun
  , parseVar
  , between (char '(') (char ')') parseLTerm
  ]

parseLTerm :: Parsec String st LTerm
parseLTerm = do
  terms <- sepBy1 parsePrimLTerm (many1 space)
  return $ foldl1 Apply terms