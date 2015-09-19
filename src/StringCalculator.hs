{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts #-}

module StringCalculator
    ( someFunc
    , htf_thisModulesTests
    ) where

import Control.Applicative
import Text.Parsec

import Test.Framework
import Test.HUnit

data Calculation = Invalid String | Valid Integer deriving (Eq, Show)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

calculate :: String -> Calculation
calculate input = case parse integer "string" input of
    Left error -> Invalid (show error)
    Right result -> Valid result

calculate' :: String -> Integer
calculate' input = case calculate input of
    Valid result -> result
    Invalid reason -> error reason

integer :: Stream s m Char => ParsecT s u m Integer
integer = read <$> many1 digit

test_zero_is_zero = 0 @=? calculate' "0"

prop_calculate_of_integer_gives_same_integer :: Positive Integer -> Bool
prop_calculate_of_integer_gives_same_integer (Positive i) = i == calculate' (show i)
