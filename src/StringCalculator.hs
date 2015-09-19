{-# OPTIONS_GHC -F -pgmF htfpp #-}

module StringCalculator
    ( someFunc
    , htf_thisModulesTests
    ) where

import Test.Framework

someFunc :: IO ()
someFunc = putStrLn "someFunc"
