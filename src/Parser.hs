{-# LANGUAGE TemplateHaskell #-}

module Parser (
    parse,
    Parser,
    ParseResult,
    ParseError,
    whole,
    digit,
    char,
    eof,
    tests,
    ) where

import           Types

import           Control.Applicative
import           Data.Char
import           Data.Either

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.Framework.TH
import           Test.QuickCheck

type ParseError = String

type ParseResult a = Either ParseError (String, a)

newtype Parser a = Parser { applyParser :: String -> ParseResult a }

instance Alternative Parser where
  empty = error "Unknown error"
  a <|> b = parserPlus a b
    where
      parserPlus :: Parser a -> Parser a -> Parser a
      parserPlus (Parser a) (Parser b) = Parser $ \input -> a input `parseResultPlus` b input
      parseResultPlus :: ParseResult a -> ParseResult a -> ParseResult a
      parseResultPlus result@(Right _) _ = result
      parseResultPlus _ result = result

instance Applicative Parser where
  pure a = Parser $ \input -> Right (input, a)
  a <*> b = parserAp a b
    where
      parserAp :: Parser (a -> b) -> Parser a -> Parser b
      parserAp (Parser pf) pa = Parser $ \input -> pf input `parseResultAp` pa
        where
          parseResultAp :: ParseResult (a -> b) -> Parser a -> ParseResult b
          parseResultAp (Right (cont, f)) (Parser p) =
            case p cont of
              Right (cont', a) -> Right (cont', f a)
              Left error       -> Left error
          parseResultAp (Left error) _ = Left error

instance Functor Parser where
  fmap = parserMap
    where
      parserMap :: (a -> b) -> Parser a -> Parser b
      parserMap f (Parser p) = Parser $ \input -> case p input of
        Right (cont, a) -> Right (cont, f a)
        Left error      -> Left error

parse :: Parser a -> String -> Either String a
parse (Parser f) input =
  case f input of
    Right (cont, a) -> Right a
    Left error      -> Left error

eof :: Parser ()
eof = Parser eof'
  where
    eof' :: String -> ParseResult ()
    eof' [] = Right ([], ())
    eof' cs = Left ("Expected end-of-input, remaining: " ++ cs)

whole :: Parser (Positive Integer)
whole = Positive <$> read <$> some digit

prop_whole :: Positive Integer -> Bool
prop_whole (Positive i) = Right (Positive i) == parse whole (show i)

digit :: Parser Char
digit = Parser digit'
  where
    digit' :: String -> ParseResult Char
    digit' [] = Left "End of input"
    digit' (c:cs)
      | isDigit c = Right (cs, c)
      | otherwise = Left (show c ++ " is not a digit")

char :: Char -> Parser Char
char c = Parser char'
  where
    char' :: String -> ParseResult Char
    char' [] = Left "End of input"
    char' (c':cs')
      | c == c' = Right (cs', c)
      | otherwise = Left (show c' ++ " is unexpected, wanted " ++ show c)

prop_char_matching :: Char -> Bool
prop_char_matching c = Right c == parse (char c) [c]

prop_char_nonmatching :: Char -> Char -> Bool
prop_char_nonmatching c c' = c == c' || isLeft (parse (char c') [c])

tests = $(testGroupGenerator)
