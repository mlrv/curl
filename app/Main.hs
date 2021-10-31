{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Arrow (first)
import Data.Bool (bool)
import GHC.OldList (intercalate)

main :: IO ()
main = undefined

-------
-- Model
-------

type Name = String

data Expr
  = ATOM Atom
  | LIST [Expr]
  deriving (Eq, Read, Show)

data Atom
  = Int Int
  | Symbol Name
  deriving (Eq, Read, Show)

-------
-- Parser
-------

newtype Parser a
  = Parser (ParseString -> Either ParseError (a, ParseString))

-- state that's carried along
-- Name: name of the source
-- (Int, Int): current location in the source
-- String: remaining string left to parse
data ParseString
  = ParseString Name (Int, Int) String
  deriving (Eq, Read, Show)

-- contains current state of parser and error msg
data ParseError
  = ParseError ParseString Error
  deriving (Eq, Read, Show)

type Error = String

instance Functor Parser where
  fmap f (Parser parser) =
    Parser (\str -> first f <$> parser str)

instance Applicative Parser where
  pure x = Parser (\str -> Right (x, str))
  (Parser p1) <*> (Parser p2) =
    Parser $
      \str -> do
        (f, rest) <- p1 str
        (x, rest') <- p2 rest
        pure (f x, rest')

instance Alternative Parser where
  empty = Parser (`throwErr` "Failed consuming input")
  (Parser p1) <|> (Parser p2) =
    Parser $
      \str -> case p1 str of
        Left _ -> p2 str
        Right res -> Right res

instance Monad Parser where
  (Parser p1) >>= f =
    Parser $
      \str -> case p1 str of
        Left err -> Left err
        Right (rs, rest) -> case f rs of
          Parser parser -> parser rest

runParser :: String -> String -> Parser a -> Either ParseError (a, ParseString)
runParser name str (Parser parser) = parser $ ParseString name (0, 0) str

throwErr :: ParseString -> String -> Either ParseError a
throwErr ps@(ParseString name (row, col) _) msg =
  Left $
    ParseError ps $
      unlines
        [ "*** " ++ name ++ ": " ++ msg,
          "* On row " ++ show row ++ ", column " ++ show col ++ "."
        ]

------
-- combinators
------

oneOf :: [Char] -> Parser Char
oneOf chars =
  Parser $ \case
    ps@(ParseString name (row, col) str) -> case str of
      [] -> throwErr ps "Can't read character of empty string"
      c : cs ->
        if c `elem` chars
          then
            let (row', col')
                  | c == '\n' = (row + 1, 0)
                  | otherwise = (row, col + 1)
             in Right (c, ParseString name (row', col') cs)
          else throwErr ps $ unlines ["Unexpected character " ++ [c], "Expecting one of: " ++ show chars]

optional :: Parser a -> Parser (Maybe a)
optional (Parser parser) =
  Parser $
    \str -> case parser str of
      Left _ -> Right (Nothing, str)
      Right (x, rest) -> Right (Just x, rest)

many :: Parser a -> Parser [a]
many parser = go []
  where
    go cs = parser >>= \c -> go (c : cs) <|> pure (reverse cs)

many1 :: Parser a -> Parser [a]
many1 parser = concat <*> many parser
  where
    concat = (:) <$> parser

------
-- specialised
------

char :: Char -> Parser Char
char c = oneOf [c]

string :: String -> Parser String
string = traverse char

space :: Parser Char
space = oneOf [' ', '\n']

spaces :: Parser String
spaces = many space

spaces1 :: Parser String
spaces1 = many1 space

withSpaces :: Parser a -> Parser a
withSpaces parser =
  spaces *> parser <* spaces

parens :: Parser a -> Parser a
parens parser =
  (withSpaces $ char '(')
    *> withSpaces parser
      <* (spaces *> char ')')

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep parser = do
  first <- optional parser
  rest <- many (sep *> parser)
  pure $ maybe rest (: rest) first

-------
-- Actual Parser
-------

parseExpr :: Parser Expr
parseExpr = (ATOM <$> parseAtom) <|> (LIST <$> parseList)

parseList :: Parser [Expr]
parseList = parens $ sepBy spaces1 parseExpr

parseAtom :: Parser Atom
parseAtom = parseInt <|> parseSymbol

parseSymbol :: Parser Atom
parseSymbol = Symbol <$> parseName

-- sequence of digits, optionally prefixed by '-'
parseInt :: Parser Atom
parseInt = do
  s <- optional $ char '-'
  num <- many1 $ oneOf "0123456789"
  let res = read $ maybe num (: num) s
  pure $ Int res

-- lowercase letters, digits, and underscores
-- the first char must be a letter
parseName :: Parser Name
parseName = (:) <$> init <*> rest
  where
    init = oneOf ['a' .. 'z']
    rest = many $ oneOf $ ['a' .. 'z'] ++ "0123456789" ++ "_"

-------
-- Run Expr Parser
-------

runExprParser :: Name -> String -> Either String Expr
runExprParser name str =
  case runParser name str (withSpaces parseExpr) of
    Left (ParseError _ errMsg) -> Left errMsg
    Right (res, _) -> Right res

-------
-- Pretty print
-------

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
