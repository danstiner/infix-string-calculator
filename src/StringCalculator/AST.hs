{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts #-}

module StringCalculator.AST
    ( calculate
    , htf_thisModulesTests
    ) where

import           Types

import           Control.Applicative
import           Data.Char

import           Test.Framework
import           Test.HUnit
import           Test.QuickCheck.Modifiers

type ParseResult a = Either String (String, a)

newtype Parser a = Parser { applyParser :: String -> ParseResult a }

instance Alternative Parser where
    empty = error "Unknown error"
    a <|> b = parserPlus a b

parserPlus :: Parser a -> Parser a -> Parser a
parserPlus (Parser a) (Parser b) = Parser $ \input ->
    a input `parseResultPlus` b input

parseResultPlus :: ParseResult a -> ParseResult a -> ParseResult a
parseResultPlus result@(Right _) _      = result
parseResultPlus _                result = result

instance Applicative Parser where
    pure a = Parser $ \input -> Right (input, a)
    a <*> b = parserAp a b

parserAp :: Parser (a -> b) -> Parser a -> Parser b
parserAp (Parser pf) pa = Parser $ \input -> pf input `parseResultAp` pa
    where
        parseResultAp :: ParseResult (a -> b) -> Parser a -> ParseResult b
        parseResultAp (Right (cont, f)) (Parser p) = case p cont of
            Right (cont', a) -> Right (cont', f a)
            Left error -> Left error
        parseResultAp (Left error) _ = Left error

instance Functor Parser where
    fmap = parserMap

parserMap :: (a -> b) -> Parser a -> Parser b
parserMap f (Parser p) = Parser $ \input ->
    case p input of
        Right (cont, a) -> Right (cont, f a)
        Left error -> Left error

calculate :: String -> Either String Rational
calculate input = do
    ast <- parse input
    Right (eval ast)

parse :: String -> Either String AST
parse = runParser ast

runParser :: Parser a -> String -> Either String a
runParser (Parser f) input = case f input of
    Right (cont, a)
        | cont == "" -> Right a
        | otherwise -> Left ("Remaining input: " ++ cont)
    Left error -> Left error

ast :: Parser AST
ast = pri2 <|> pri1 <|> pri3
    where
        pri1 = multiply <|> divide
        pri2 = add <|> subtract
        pri3 = parenthesized <|> literal <|> negation
        parenthesized = char '(' *> ast <* char ')'
        multiply = Multiply <$> multdivleft <* char '*' <*> multdivright
        divide   = Divide   <$> multdivleft <* char '/' <*> multdivright
        multdivleft = pri3
        multdivright = pri1 <|> pri3
        add      = Add      <$> addsubleft <* char '+' <*> ast
        subtract = Subtract <$> addsubleft <* char '-' <*> ast
        addsubleft = pri1 <|> pri3
        literal = LiteralInteger <$> whole
        negation = Negate <$> (char '-' *> pri3)

whole :: Parser (Positive Integer)
whole = fmap (Positive . digitsToInteger) (some digit)
  where
    digitsToInteger = sum . zipWith (*) powersOfTen . reverse . map toInteger
    powersOfTen = map (10^) [0..]

digit :: Parser Int
digit = Parser digit'
  where
    digit' :: String -> ParseResult Int
    digit' [] = Left "End of input"
    digit' (c:cs)
        | isDigit c = Right (cs, digitToInt c)
        | otherwise = Left (show c ++ " is not a digit")

char :: Char -> Parser Char
char c = Parser char'
    where
        char' :: String -> ParseResult Char
        char' [] = Left "End of input"
        char' (c':cs')
            | c == c' = Right (cs', c)
            | otherwise = Left (show c' ++ " is not expected " ++ show c)

eval :: AST -> Rational
eval (LiteralInteger (Positive i)) = fromIntegral i
eval (Subtract left right)         = eval left - eval right
eval (Add left right)              = eval left + eval right
eval (Divide dividend divisor)     = eval dividend / eval divisor
eval (Multiply left right)         = eval left * eval right
eval (Negate ast)                  = -1 * eval ast
