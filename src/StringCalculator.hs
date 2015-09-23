module StringCalculator
    ( implementations
    , calculate
    ) where

import qualified StringCalculator.AST as AST

implementations :: [(String, (String -> Either String Rational))]
implementations = [("AST", AST.calculate)]

calculate = AST.calculate
