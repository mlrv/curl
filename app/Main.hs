{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Arrow (first)

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
