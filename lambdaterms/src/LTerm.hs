{-# LANGUAGE LambdaCase #-}

module LTerm (LTerm(..)) where

data LTerm = Var String | Fun String LTerm | Apply LTerm LTerm
  deriving Eq

traverseFun :: LTerm -> ([String], LTerm)
traverseFun (Fun a inner@(Fun _ _)) = let (vars, innermost) = traverseFun inner in (a : vars, innermost)
traverseFun (Fun a innermost) = ([a], innermost)
traverseFun _ = ([], Var "eps")

traverseApply :: LTerm -> [LTerm]
traverseApply (Apply inner@(Apply _ _) b) = traverseApply inner ++ [b]
traverseApply (Apply a b) = [a, b]
traverseApply _ = []

instance Show LTerm where
  show (Var a) = a
  show t@(Fun _ _) = let (vars, innermost) = traverseFun t in ('\\' : unwords vars ++ "." ++ show innermost)
  show t@(Apply _ _) = let terms = traverseApply t in unwords $ 
    map (\case 
          it@(Var _) -> show it
          other -> '(' : show other ++ ")"
        ) terms