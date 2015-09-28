module Main where

import qualified Parser
import qualified StringCalculator.AST
import qualified StringCalculator.Prefix
import           Test.Framework          (defaultMain)

main :: IO ()
main = defaultMain [Parser.tests, StringCalculator.Prefix.tests]
