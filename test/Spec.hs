{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import           Lexer
import           StringCalculator.AST
import           Test.Framework

main :: IO ()
main = htfMain htf_importedTests
