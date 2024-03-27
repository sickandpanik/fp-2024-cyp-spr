{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Parser where 

import Data.Char ( isAlpha, isAlphaNum, isDigit, digitToInt )
import Control.Applicative ( Alternative((<|>), empty, many) )

keywords :: [String]
keywords = ["if", "then", "else"]

-- This is a straightforward implementation of an indetifier parser. 
parseIdent :: String -> Either String String 
parseIdent str@(h:t) 
    | isValidFirst h && all isValid t = 
        if str `notElem` keywords 
        then Right str 
        else Left "Keyword cannot be an identifier"
    | otherwise = Left "Lexical error: inadmissible character"
  where 
    isValidFirst x = x == '_' || x == '\'' || isAlpha x 
    isValid x = isAlphaNum x || isValidFirst x 
parseIdent [] = Left "Empty string is not an identifier"

-- This is a straightforward implementation of a number parser. 
parseInt :: String -> Either String Int 
parseInt (h:t) = 
    if h == '-'
    then ((-1) *) <$> parseNumber t 
    else parseNumber (h:t)
  where 
    parseNumber [] = Left "Empty string is not a number"
    parseNumber str = 
        if all isDigit str
        then Right $ toDigit str 
        else Left "Lexical error: inadmissible character"
      where 
        toDigit = foldl1 (\a x -> a * 10 + x) . map digitToInt
parseInt [] = Left "Empty string is not a number"

-- It's not clear how to compose the parsers above, so people usually use a different abstraction for a parser. 
-- A parser consumes the prefix of an input String while it's a valid string of the language being parsed. 
-- Then the unread suffix is returned along with the result of the parsing. 
-- The result may be a string (for identifiers), an integer (for numbers), 
-- some algebraic data type for more complex langugaes (for example, Expr for expressions), 
-- or even a function. 
newtype Parser a 
  = Parser { runParser :: String -> Maybe (String, a)}

-- This abstraction of a parser is a Functor, which allows us to transform the parser's results. 
instance Functor Parser where 
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ \input -> 
    case runParser p input of 
      Nothing -> Nothing 
      Just (suff, r) -> Just (suff, f r) 
      
-- The parser is also an applicative functor, which simplifies composition.       
instance Applicative Parser where
  pure :: a -> Parser a
  pure res = Parser $ \str -> Just (str, res)
  
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) f p = Parser $ \str ->
    case runParser f str of
      Just (str', f') ->
        case runParser p str' of
          Just (str'', a) -> Just (str'', f' a)
          Nothing -> Nothing
      Nothing -> Nothing
    
-- Monadic bind is something which expresses the idea of sequential parser application. 
-- First parse the string with this parser, and then parse the rest with that parser.  
-- This is one of two most important operations on parsers.    
instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) f p = Parser $ \str ->
    case runParser f str of
      Just (str', res) -> runParser (p res) str'
      Nothing -> Nothing

-- Alternative operation allows us to express that something is either this or that. 
-- Note that it favors the left-hand parser: if it succeeds consuming any prefix of the input string, 
-- the right parser will not be tried. 
instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing -- a parser which always reports an error: no strings in its language.

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) l r = Parser $ \str ->
    case runParser l str of
      Just (str', res) -> Just (str', res)
      Nothing -> runParser r str

-- This function creates a parser which checks that a predicate holds for the first character of an input string.  
satisfy :: (Char -> Bool) -> Parser Char 
satisfy p = Parser $ \str -> 
  case str of 
    (h:t) | p h -> Just (t, h) 
    _ -> Nothing

-- Ident = Alpha AlphaNum*
-- starts with letter 
-- continues with a sequence (possibly empty) of letter or digits
parseIdent' :: Parser String
parseIdent' = do 
    h <- satisfy isAlpha -- the first character is a letter
    t <- go              -- then follows a sequence of letters or digits
    return (h : t)       -- we compose the result 
  where 
    go = (do                    -- a sequence of symbols is a symbol followed by the sequence or an empty sequence. 
        x <- satisfy isAlphaNum -- the first symbol is either a letter or a digit
        y <- go                 -- then goes the sequence
        return (x : y))         -- and we return the result 
      <|>  
        return []               -- note that we only parse the empty sequence of letters after we've tried to match the non-empty sequence 
                                -- experiment by switching the order and see what happens then. 

-- This function creates a parser for something surrounded by parentheses. 
-- Both parentheses and the "something" are parameters. 
inParens :: Char -> Char -> Parser a -> Parser a 
inParens l r p = do 
  satisfy (== l) -- we don't bind the result of parsing here (btw, what is its type?) because we don't need it
  x <- p         -- the only thing we need is the result of parsing the "something"
  satisfy (== r) -- throw away this result also 
  return x       -- no parentheses made it into the result 

-- A tuple is a sequence of identifiers, separated by ',' in parentheses
parseIdentTuple :: Parser [String]
parseIdentTuple = inParens '(' ')' identSequence 

-- This parser not only parses the list of identifiers, but also computes their lengths
parseIdentList :: Parser [Int]
parseIdentList = map length <$> inParens '[' ']' identSequence 

-- A pair is two identifiers with a comma in between, surrounded by parentheses
parsePair :: Parser (String, String) 
parsePair = inParens '(' ')' $ do  
  x <- parseIdent' 
  satisfy (== ',')
  y <- parseIdent'
  return (x, y)

-- This is the same parser as parsePair written in the applicative style
-- <* functions the same ways as does <*> but ignores its result on the right
parsePair' :: Parser (String, String) 
parsePair' = inParens '(' ')' 
  ((,) <$> parseIdent' <* satisfy (== ',') <*> parseIdent')

-- A sequence of identifiers separated by commas is: 
-- a single identifier followed by a sequence of comma-identifier pairs
-- or an empty sequence. 
-- Here we use the function `many :: Alternative f => f a -> f [a]` which applies its argument multiple times (until the first failure)
-- and then collects the results in the list. 
-- The function `>>` is the same as `>>=`, but it ignores its left result, only performing the effect. 
identSequence :: Parser [String]
identSequence = (do
    h <- parseIdent' 
    t <- many (satisfy (== ',') >> parseIdent') 
    return (h : t)
  )
  <|> 
    return [] 

