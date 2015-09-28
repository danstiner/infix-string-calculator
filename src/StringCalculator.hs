module StringCalculator (implementations, calculate) where

import qualified StringCalculator.AST    as AST
import qualified StringCalculator.Prefix as Prefix

implementations :: [(String, String -> Either String Rational)]
implementations =
  [("AST", AST.calculate), ("Prefix", Prefix.calculate)]

calculate = Prefix.calculate
