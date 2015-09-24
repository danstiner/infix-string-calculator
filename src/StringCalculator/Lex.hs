{-# LANGUAGE FlexibleContexts #-}

module StringCalculator
    ( someFunc
    ) where

import           Lexer
import           Types

import           Control.Applicative       ((<$>))
import           Data.DeriveTH
import           Text.Parsec

import           Test.Framework
import           Test.HUnit
import           Test.QuickCheck.Modifiers

data Calculation = Invalid String | Valid Integer deriving (Eq, Show)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

calculate :: String -> Calculation
calculate input = case lexString input of
    Left error -> Invalid (show error)
    Right [IntegerToken (Positive result)] -> Valid result

infixToPostfix :: [Token] -> [Token]
infixToPostfix = undefined

evaluatePostFix :: [Token] -> ([Token], Integer)
evaluatePostFix (IntegerToken (Positive i) : ts) = (ts, i)
evaluatePostFix (Minus : ts) = undefined

calculate' :: String -> Integer
calculate' input = case calculate input of
    Valid result -> result
    Invalid reason -> error reason

test_zero_is_zero = 0 @=? calculate' "0"

prop_calculate_of_integer_gives_same_integer :: Positive Integer -> Bool
prop_calculate_of_integer_gives_same_integer (Positive i) = i == calculate' (show i)
