module Pretty where

import Data.Bool (bool)
import GHC.OldList (intercalate)
import Model (Atom (..), Expr (..), Name)

printExpr :: Expr -> String
printExpr = printExpr' False 0

-- TODO incorrect function call?
printExpr' :: Bool -> Int -> Expr -> String
printExpr' doIndent level = \case
  ATOM a -> indent (bool 0 level doIndent) (printAtom a)
  LIST (e : es) ->
    indent (bool 0 level doIndent) $
      concat
        [ "(",
          printExpr' False (level + 1) e,
          bool "\n" "" (null es),
          intercalate "\n" $ map (printExpr' True (level + 1)) es,
          ")"
        ]

printAtom :: Atom -> String
printAtom = \case
  Symbol s -> s
  Int i -> show i

indent :: Int -> String -> String
indent tabs s = concat (replicate tabs "  ") ++ s