{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Lexer
    ( lexString
    , tests
    , Token (..)
    ) where

import           Control.Applicative                  ((<$>))
import           Data.DeriveTH
import           Text.Parsec

import           Test.Framework                       (testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.Framework.TH
import           Test.HUnit
import           Test.QuickCheck
import           Test.QuickCheck.Modifiers

data Calculation = Invalid String | Valid Integer deriving (Eq, Show)

data Token = IntegerToken (Positive Integer) | Minus | Plus | LeftParen | RightParen | Divide | Times deriving (Eq, Show)

$( derive makeArbitrary ''Token )

integer :: Stream s m Char => ParsecT s u m (Positive Integer)
integer = Positive <$> read <$> lexeme (many1 digit)

lexString :: String -> Either ParseError [Token]
lexString = parse lexer "string"

lexer :: Stream s m Char => ParsecT s u m [Token]
lexer = many1 tokenP

tokenP :: Stream s m Char => ParsecT s u m Token
tokenP = (IntegerToken <$> integer) <|> minus <|> plus <|> leftParen <|> rightParen <|> divide <|> times

minus, plus, leftParen, rightParen, divide, times :: Stream s m Char => ParsecT s u m Token
minus = symbolc '-' >> return Minus
plus = symbolc '+' >> return Plus
leftParen = symbolc '(' >> return LeftParen
rightParen = symbolc ')' >> return RightParen
divide = symbolc '/' >> return Divide
times = symbolc '*' >> return Times

symbolc :: Stream s m Char => Char -> ParsecT s u m Char
symbolc c = lexeme (char c)

lexeme p = do
    val <- p
    spaces
    return val

prop_tokenize :: NonEmptyList Token -> Bool
prop_tokenize (NonEmpty tokens) = tokens == either (error . show) id (lexString (printToken tokens))

printToken :: [Token] -> String
printToken [] = ""
printToken (IntegerToken (Positive i) : t@(IntegerToken (Positive j)) : ts) = show i ++ " " ++ printToken (t:ts)
printToken (IntegerToken (Positive i) : ts) = show i  ++ printToken ts
printToken (Minus : ts) = "-" ++ printToken ts
printToken (Plus : ts) = "+" ++ printToken ts
printToken (LeftParen : ts) = "(" ++ printToken ts
printToken (RightParen : ts) = ")" ++ printToken ts
printToken (Divide : ts) = "/" ++ printToken ts
printToken (Times : ts) = "*" ++ printToken ts

tests = $(testGroupGenerator)
