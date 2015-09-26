{-# LANGUAGE TemplateHaskell       #-}

module StringCalculator.Postfix
    ( calculate
    ) where

import           Parser                    (Parser, ParseError, char, whole)
import qualified Parser
import           Types

import           Control.Monad
import           Data.DeriveTH
import           Control.Applicative
import           Data.Char
import           Test.Framework.TH
import           Test.QuickCheck
import           Test.QuickCheck.Modifiers

data Token = IntegerToken (Positive Integer) | Minus | Plus | LeftParen | RightParen | Divide | Times deriving (Eq, Show)

newtype Postfix = Postfix [Token]

$(derive makeArbitrary ''Token)

calculate :: String -> Calculation
calculate = eval <=< toPostfix <=< lexString

lexString :: String -> Either ParseError [Token]
lexString = Parser.parse lexer

lexer :: Parser [Token]
lexer = some token

token :: Parser Token
token = wholeTok <|> minus <|> plus <|> leftParen <|> rightParen <|> divide <|> times

wholeTok, minus, plus, leftParen, rightParen, divide, times :: Parser Token
wholeTok = IntegerToken <$> whole
minus = char '-' *> pure Minus
plus = char '+' *> pure Plus
leftParen = char '(' *> pure LeftParen
rightParen = char ')' *> pure RightParen
divide = char '/' *> pure Divide
times = char '*' *> pure Times

toPostfix :: [Token] -> Either String Postfix
toPostfix (IntegerToken (Positive i) : []) = Right (Postfix [IntegerToken (Positive i)])
toPostfix ts = Left ("Unexpected input: " ++ show ts)

eval :: Postfix -> Calculation
eval (Postfix (IntegerToken (Positive i) : [])) = Right (fromIntegral i)
eval (Postfix ts) = Left ("Unexpected: " ++ show ts)


tests = $(testGroupGenerator)
