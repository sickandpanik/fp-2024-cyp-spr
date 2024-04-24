module Main (main) where

import LTerm
import LTermParser
import Text.Parsec (parse)

main :: IO ()
main = do
  putStrLn "Please enter your lambda term. You can omit unnecessary parentheses "
  putStrLn "but please separate variables names with one or more spaces."
  lTermString <- getLine
  let parsedLTerm = parse parseLTerm "" lTermString
  putStrLn "Here is how we parsed (and pretty-printed) it:"
  print parsedLTerm