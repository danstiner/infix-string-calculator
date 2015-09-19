{-# OPTIONS_GHC -F -pgmF htfpp #-}

module StringCalculator
    ( someFunc
    , htf_thisModulesTests
    ) where

import Test.Framework
import Test.HUnit

data Calculation = Error String | Valid Integer deriving (Eq, Show)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

calculate :: String -> Calculation
calculate _ = Valid 0

test_zero_is_zero = Valid 0 @=? calculate "0"
