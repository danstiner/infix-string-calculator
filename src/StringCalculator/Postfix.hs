{-# LANGUAGE TemplateHaskell #-}

module StringCalculator.Postfix
    ( calculate
    , tests
    ) where

import           Parser                               hiding (tests)
import           Types

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.DeriveTH
import           Data.Word
import           Test.Fluent
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.Framework.TH
import           Test.QuickCheck
import           Test.QuickCheck.Modifiers
import           Test.QuickCheck.Property             as Property

data Token = IntegerToken (Positive Integer) | Minus | Plus | LeftParen | RightParen | DivideTok | Times | NegateTok deriving (Eq)

instance Show Token where
    show (IntegerToken (Positive i)) = show i
    show Minus = "-"
    show Plus = "+"
    show LeftParen = "("
    show RightParen = ")"
    show DivideTok = "/"
    show Times = "*"
    show NegateTok = "-"

newtype Postfix = Postfix [Token]

$(derive makeArbitrary ''Token)

calculate :: String -> Calculation
calculate = eval <=< toPostfix <=< lexString

lexString :: String -> Either ParseError [Token]
lexString = parse (lexer <* eof)

lexer :: Parser [Token]
lexer = (++) <$> many negation <*> (concat <$> some token)

token :: Parser [Token]
token = wholeTok' <|> opThenNegation <|> operator' <|> rightParen'
    where
        wholeTok' = (:[]) <$> wholeTok
        operator' = (:[]) <$> operator
        rightParen' = (:[]) <$> rightParen

opThenNegation :: Parser [Token]
opThenNegation = (:) <$> (operator <|> leftParen) <*> many negation

negation :: Parser Token
negation = char '-' *> pure NegateTok

operator :: Parser Token
operator = minus <|> plus <|> divide <|> times

wholeTok, minus, plus, leftParen, rightParen, divide, times :: Parser Token
wholeTok = IntegerToken <$> whole
minus = char '-' *> pure Minus
plus = char '+' *> pure Plus
leftParen = char '(' *> pure LeftParen
rightParen = char ')' *> pure RightParen
divide = char '/' *> pure DivideTok
times = char '*' *> pure Times

toPostfix :: [Token] -> Either String Postfix
toPostfix [IntegerToken (Positive i)] = Right (Postfix [IntegerToken (Positive i)])
toPostfix ts = Left ("Unexpected input: " ++ show ts)

eval :: Postfix -> Calculation
eval (Postfix [IntegerToken (Positive i)]) = Right (fromIntegral i)
eval (Postfix ts) = Left ("Unexpected: " ++ show ts)

prop_tokenize_an_integer :: Positive Integer -> Property.Result
prop_tokenize_an_integer (Positive i) = Right [IntegerToken (Positive i)] `shouldBe` lexString (show i)

prop_tokenize_top_level_negation :: Int -> Property.Result
prop_tokenize_top_level_negation count =
    lexString (tokensToString tokens) `shouldBe` Right tokens
    where
        tokens = replicate count NegateTok ++ [one]
        one = IntegerToken (Positive 1)

prop_tokenize_ast :: AST -> Property.Result
prop_tokenize_ast ast =
    lexString (tokensToString tokens) `shouldBe` Right tokens
    where
        tokens = astToTokens ast

astToTokens :: AST -> [Token]
astToTokens (LiteralInteger p) = [IntegerToken p]
astToTokens (Subtract a b) = astToTokens a ++ [Minus] ++ astToTokens b
astToTokens (Add a b) = astToTokens a ++ [Plus] ++ astToTokens b
astToTokens (Divide a b) = astToTokens a ++ [DivideTok] ++ astToTokens b
astToTokens (Multiply a b) = astToTokens a ++ [Times] ++ astToTokens b
astToTokens (Negate ast) = NegateTok : astToTokens ast

tokensToString :: [Token] -> String
tokensToString = concatMap show

tests = $(testGroupGenerator)
