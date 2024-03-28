module Main (main) where

import Lib
import Parser
import Expr
import ExprParser
import StateDemo
import Error

import Data.Map.Strict (Map, map, empty)
import System.IO
import Text.Read (readMaybe)
import Text.Printf (printf)

main :: IO ()
main = do
  putStrLn "♪ Expressions! ♪"
  putStrLn "\nPlease enter your expression in prefix form,\nseparating functions (+, -, *, /, ^, sqrt)\nfrom their arguments by exactly one space."
  putStrLn "\nExamples:\n\t'+ 2 2'\n\t'* 2 * pi r'"
  exprLine <- promptLine "> "
  let parseResult = runParser parseExpr exprLine
  case parseResult of
    Right (_, expr) -> (do
      maybeState <- getExprState expr
      case maybeState of
        Just state -> putStrLn $ displayEvalResult $ execState (eval expr) state
        Nothing -> putStrLn "Error while parsing variable bindings!"
      )
    Left _ -> putStrLn "Error while parsing expression!"

promptLine :: String -> IO String
promptLine prompt = do
  putStr prompt
  hFlush stdout
  getLine

getExprState :: (Num a) => Expr a -> IO (Maybe (Map String a))
getExprState e = if containsVariables e then (do
    putStrLn "\nPlease enter variable bindings as if\nyou were entering a [(String, Integer)] value."
    putStrLn "\nExample:\n\t'[(\"a\", 2), (\"b\", 3)]' supplied with expression '- a b'\n\twill yield -1.0."
    listLine <- promptLine "> "
    let maybeState = readMaybe ("fromList " ++ listLine) :: Maybe (Map String Integer)
    return $ fmap (Data.Map.Strict.map fromIntegral) maybeState
  ) else return $ Just empty

displayEvalResult :: (Show a) => Either (Error a) a -> String
displayEvalResult (Left (Error kind expr)) = printf "Error %s while evaluating '%s'!" (show kind) (showExprPrefix expr)
displayEvalResult (Right x) = printf "Your expression evaluates to %s" (show x)

showExprPrefix :: (Show a) => Expr a -> String
showExprPrefix expr = case expr of
  Const x -> show x
  Var varName -> varName
  Operation (Sqrt e) -> "sqrt " ++ showExprPrefix e
  Operation (BinOp op e1 e2) -> printf "%s %s %s" (show op) (showExprPrefix e1) (showExprPrefix e2)