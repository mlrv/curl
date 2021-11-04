module JavaScript where

import GHC.OldList (intercalate)
import Model (Atom (..), Expr (..), Name)
import Pretty (indent)

-- model

data JSExpr
  = JSInt Int -- ATOM Int
  | JSSymbol Name -- ATOM Symbol
  | JSBinOp JSBinOp JSExpr JSExpr -- binary expr (*, +, etc.)
  | JSLambda [Name] JSExpr -- anonymous functions
  | JSFunCall JSExpr [JSExpr] -- function calls
  | JSReturn JSExpr -- return values from functions
  deriving (Eq, Show, Read)

type JSBinOp = String

-- codegen

printJSExpr :: Bool -> Int -> JSExpr -> String
printJSExpr doIndent tabs = \case
  JSInt i -> show i
  JSSymbol name -> name
  JSBinOp op e1 e2 ->
    parensConcat
      [ printNoIndent e1,
        " ",
        printJSOp op,
        " ",
        printNoIndent e2
      ]
  JSLambda vars expr ->
    indentIf $
      unlines
        [ "function(" ++ intercalate ", " vars ++ ") {",
          indent (tabs + 1) $ printJSExpr False (tabs + 1) expr
        ]
        ++ indent tabs "}"
  JSFunCall f exprs ->
    parensConcat
      [ printNoIndent f,
        ")(",
        intercalate ", " (printNoIndent <$> exprs)
      ]
  JSReturn expr -> indentIf $ "return " ++ printNoIndent expr ++ ";"
  where
    indentIf = if doIndent then indent tabs else id
    printNoIndent = printJSExpr False tabs
    parensConcat = parens . mconcat
    parens = \s -> "(" ++ s ++ ")"

printJSOp :: JSBinOp -> String
printJSOp op = op