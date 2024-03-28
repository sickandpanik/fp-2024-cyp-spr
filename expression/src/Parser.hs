{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Parser where 

import Data.Char ( isAlpha, isAlphaNum, isDigit, digitToInt )
import Control.Applicative ( Alternative((<|>), empty, many, some) )

import ParsingError

-- It's not clear how to compose the parsers above, so people usually use a different abstraction for a parser. 
-- A parser consumes the prefix of an input String while it's a valid string of the language being parsed. 
-- Then the unread suffix is returned along with the result of the parsing. 
-- The result may be a string (for identifiers), an integer (for numbers), 
-- some algebraic data type for more complex langugaes (for example, Expr for expressions), 
-- or even a function. 
newtype Parser a 
  = Parser { runParser :: String -> Either ParsingError (String, a)}

-- This abstraction of a parser is a Functor, which allows us to transform the parser's results. 
instance Functor Parser where 
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ \input -> 
    case runParser p input of
      Right (suff, r) -> Right (suff, f r) 
      Left error -> Left error
      
-- The parser is also an applicative functor, which simplifies composition.       
instance Applicative Parser where
  pure :: a -> Parser a
  pure res = Parser $ \str -> Right (str, res)
  
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) f p = Parser $ \str ->
    case runParser f str of
      Right (str', f') ->
        case runParser p str' of
          Right (str'', a) -> Right (str'', f' a)
          Left error -> Left error
      Left error -> Left error
    
-- Monadic bind is something which expresses the idea of sequential parser application. 
-- First parse the string with this parser, and then parse the rest with that parser.  
-- This is one of two most important operations on parsers.    
instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) f p = Parser $ \str ->
    case runParser f str of
      Right (str', res) -> runParser (p res) str'
      Left error -> Left error

-- Alternative operation allows us to express that something is either this or that. 
-- Note that it favors the left-hand parser: if it succeeds consuming any prefix of the input string, 
-- the right parser will not be tried. 
instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const (Left EmptyLanguage) -- a parser which always reports an error: no strings in its language.

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) l r = Parser $ \str ->
    case runParser l str of
      Right (str', res) -> Right (str', res)
      Left _ -> runParser r str

errorParser :: ParsingError -> Parser a
errorParser error = Parser $ const (Left error)

-- This function creates a parser which checks that a predicate holds for the first character of an input string.  
satisfy :: (Char -> Bool) -> Parser Char 
satisfy p = Parser $ \str -> 
  case str of 
    (h:t) | p h -> Right (t, h) 
    _ -> Left Unexpected

