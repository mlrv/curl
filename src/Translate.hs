module Translate where

import Control.Arrow ((***))
import JavaScript (JSExpr (..))
import Model (Atom (..), Expr (..), Name)
import Parse (Error)
import Pretty (printExpr)

type TranslError = String

type Builtin = [Expr] -> Either TranslError JSExpr

type Builtins = [(Name, Builtin)]

translateToJS :: Expr -> Either TranslError JSExpr
translateToJS = \case
  ATOM (Symbol s) -> pure $ JSSymbol s
  ATOM (Int i) -> pure $ JSInt i
  LIST xs -> translateList xs

translateList :: [Expr] -> Either TranslError JSExpr
translateList = \case
  [] -> Left "Translating empty list"
  ATOM (Symbol s) : xs
    | Just f <- lookup s builtins -> f xs
  f : xs -> JSFunCall <$> translateToJS f <*> traverse translateToJS xs

builtins :: Builtins
builtins =
  [ ("lambda", translLambda),
    ("let", translLet),
    ("add", translBinOp "add" "+"),
    ("mul", translBinOp "mul" "*"),
    ("sub", translBinOp "sub" "-"),
    ("div", translBinOp "div" "/"),
    ("print", translPrint)
  ]

translLambda :: [Expr] -> Either TranslError JSExpr
translLambda = \case
  [] -> Left "Translating empty list"
  [LIST vars, body] -> do
    vars' <- traverse fromSymbol vars
    JSLambda vars' <$> (JSReturn <$> translateToJS body)
  vars ->
    Left $
      unlines
        [ "Syntax error: unexpected arguments for lambda.",
          "expecting 2 arguments, the first is the list of vars and the second is the body of the lambda.",
          "In expression: " ++ show (LIST $ ATOM (Symbol "lambda") : vars)
        ]

translLet :: [Expr] -> Either TranslError JSExpr
translLet = \case
  [] -> Left "???"
  [LIST binds, body] -> do
    (vars, vals) <- letParams binds
    vars' <- traverse fromSymbol vars
    JSFunCall . JSLambda vars' <$> (JSReturn <$> translateToJS body) <*> traverse translateToJS vals
  vars ->
    Left $
      unlines
        [ "Syntax error: unexpected arguments for let.",
          "expecting 2 arguments, the first is the list of var/val pairs and the second is the let body.",
          "In expression:\n" ++ printExpr (LIST $ ATOM (Symbol "let") : vars)
        ]
  where
    letParams :: [Expr] -> Either Error ([Expr], [Expr])
    letParams = \case
      [] -> pure ([], [])
      LIST [x, y] : rest -> ((x :) *** (y :)) <$> letParams rest
      x : _ -> Left ("Unexpected argument in let list in expression:\n" ++ printExpr x)

translBinOp :: Name -> Name -> [Expr] -> Either TranslError JSExpr
translBinOp f _ [] = Left $ "Syntax error: " ++ f ++ " expected at least 1 argument, got 0"
translBinOp _ _ [x] = translateToJS x
translBinOp _ f list = foldl1 (JSBinOp f) <$> traverse translateToJS list

translPrint :: [Expr] -> Either TranslError JSExpr
translPrint [expr] = JSFunCall (JSSymbol "console.log") . (: []) <$> translateToJS expr
translPrint xs = Left $ "Syntax error: print expected 1 arguments, got " ++ show (length xs)

fromSymbol :: Expr -> Either String Name
fromSymbol (ATOM (Symbol s)) = Right s
fromSymbol e = Left $ "Can't bind value to non symbol type: " ++ show e