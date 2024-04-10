module ExprParser where

import Data.Char ( isAlpha, isAlphaNum, isDigit, digitToInt )
import Control.Applicative ( Alternative((<|>), empty, many, some) )

import Expr
import Parser
import ParsingError

delim = " "

binOperatorsString = concatMap show (enumFrom minBound :: [BinOperator])

binOperatorsMap = map (\x -> (show x, x)) (enumFrom minBound :: [BinOperator])

parseDigit :: Parser Char
parseDigit = satisfy isDigit

parseIdentChar :: Parser Char
parseIdentChar = satisfy $ \c -> c `notElem` delim ++ binOperatorsString

digitsToNumber :: String -> Int
digitsToNumber = foldl1 (\a x -> a * 10 + x) . map digitToInt

parseConst :: Parser (Expr Double)
parseConst = do
  digits <- some parseDigit
  let number = fromIntegral $ digitsToNumber digits
  return (Const number)

parseIdent :: Parser String
parseIdent = do
  h <- satisfy isAlpha
  t <- many parseIdentChar
  return (h:t)

parseVar :: Parser (Expr Double)
parseVar = do
  ident <- parseIdent
  case ident of
    "sqrt" -> errorParser Unexpected
    _ -> return $ Var ident

parseUnaryOp :: Parser String
parseUnaryOp = do
  ident <- parseIdent
  case ident of
    "sqrt" -> return ident
    _ -> errorParser Unexpected

parseBinOperator :: Parser BinOperator
parseBinOperator = do
  opString <- satisfy $ \c -> c `elem` binOperatorsString
  case lookup [opString] binOperatorsMap of
    Just op -> return op
    Nothing -> errorParser Unexpected

parseExpr :: Parser (Expr Double)
parseExpr = parseConst <|> parseVar <|>
  (do
    parseUnaryOp
    satisfy (== ' ')
    nestedExpr <- parseExpr
    return $ Operation (Sqrt nestedExpr)
  )
  <|>
  (do
    op <- parseBinOperator
    satisfy (== ' ')
    lhs <- parseExpr
    satisfy (== ' ')
    rhs <- parseExpr
    return $ Operation (BinOp op lhs rhs)
  )
