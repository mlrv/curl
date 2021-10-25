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

-- contains current state of parser and error msg
data ParseError
  = ParseError ParseString Error

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