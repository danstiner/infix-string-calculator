module Main where

import qualified Lexer
import qualified Parser
import qualified StringCalculator.AST
import           Test.Framework       (defaultMain)

main :: IO ()
main = defaultMain
    [ Lexer.tests
    , Parser.tests
    ]
