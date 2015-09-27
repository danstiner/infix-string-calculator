module Main where

import qualified Parser
import qualified StringCalculator.AST
import qualified StringCalculator.Postfix
import           Test.Framework           (defaultMain)

main :: IO ()
main = defaultMain
    [ Parser.tests
    , StringCalculator.Postfix.tests
    ]
