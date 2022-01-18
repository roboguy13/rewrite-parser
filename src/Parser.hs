{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wall #-}

module Parser
  (Parser
  ,Parseable
  ,getParser
  ,parse
  ,noParse
  ,parseCharWhen
  ,parseChar
  ,parseOneOf
  ,parseNotOneOf
  ,parseString
  ,parseSpace
  ,parseNewline
  ,parseWhitespace
  ,parseBracketed
  ,parseSequence

  ,module Control.Applicative
  )
  where

import           Control.Applicative
import           Control.Monad

import           Data.Void

newtype Parser a = MkParser { runParser :: String -> [(String, a)] }

data ParseError = ParseError | AmbiguousParseError | TrailingCharsError
  deriving (Show)

class Parseable a where
  getParser :: Parser a

parse :: Parser a -> String -> Either ParseError a
parse p str =
  case runParser p str of
    [] -> Left ParseError
    [("", r)] -> Right r
    [(_, _)] -> Left TrailingCharsError
    _ -> Left AmbiguousParseError

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return x = MkParser (\str -> pure (str, x))
  MkParser p >>= f =
    MkParser $ \str0 -> do
      (str, x) <- p str0
      runParser (f x) str

instance Alternative Parser where
  empty = MkParser (const empty)
  p <|> q = MkParser (\str -> runParser p str <|> runParser q str)

noParse :: Parser Void
noParse = empty

parseCharWhen :: (Char -> Bool) -> Parser Char
parseCharWhen cond = MkParser $ \case
  ""            -> empty
  (c:cs)
    | cond c    -> pure (cs, c)
    | otherwise -> empty

parseChar :: Char -> Parser Char
parseChar c = parseCharWhen (== c)

parseOneOf :: [Char] -> Parser Char
parseOneOf cs = parseCharWhen (`elem` cs)

parseNotOneOf :: [Char] -> Parser Char
parseNotOneOf cs = parseCharWhen (not . (`elem` cs))

parseString :: String -> Parser String
parseString str = mapM parseChar str

parseSpace :: Parser Char
parseSpace = parseOneOf " \t"

parseNewline :: Parser Char
parseNewline = parseOneOf "\n\r"

-- | Parse space or newline
parseWhitespace :: Parser Char
parseWhitespace = parseSpace <|> parseNewline

parseBracketed :: Parser a -> Parser a -> Parser b -> Parser b
parseBracketed start end p = start *> p <* end

parseSequence :: Parser a -> Parser b -> Parser [b]
parseSequence sep p = go
  where
    go = fmap (:[]) p <|> do
      x <- p
      _ <- sep
      xs <- go
      pure (x:xs)

