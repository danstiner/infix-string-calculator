{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module StringCalculator
    ( someFunc
    , htf_thisModulesTests
    ) where

import Control.Applicative ((<$>))
import Text.Parsec
import Data.DeriveTH

import Test.Framework
import Test.HUnit
import Test.QuickCheck.Modifiers

data Calculation = Invalid String | Valid Integer deriving (Eq, Show)

data Token = IntegerToken (Positive Integer) | Minus | Plus | LeftParen | RightParen | Divide | Times deriving (Eq, Show)

$( derive makeArbitrary ''Token )

someFunc :: IO ()
someFunc = putStrLn "someFunc"

calculate :: String -> Calculation
calculate input = case parse integer "string" input of
    Left error -> Invalid (show error)
    Right (Positive result) -> Valid result

calculate' :: String -> Integer
calculate' input = case calculate input of
    Valid result -> result
    Invalid reason -> error reason

integer :: Stream s m Char => ParsecT s u m (Positive Integer)
integer = Positive <$> read <$> lexeme (many1 digit)

lexInput :: String -> Either ParseError [Token]
lexInput = parse (lexer) "string"

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

test_zero_is_zero = 0 @=? calculate' "0"

prop_calculate_of_integer_gives_same_integer :: Positive Integer -> Bool
prop_calculate_of_integer_gives_same_integer (Positive i) = i == calculate' (show i)

prop_tokenize :: NonEmptyList Token -> Bool
prop_tokenize (NonEmpty tokens) = tokens == either (error . show) id (lexInput (printToken tokens))

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
