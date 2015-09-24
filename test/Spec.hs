module Main where

import qualified Lexer
import qualified StringCalculator.AST
import           Test.Framework       (defaultMain)

main :: IO ()
main = defaultMain
	[ Lexer.tests
	]
