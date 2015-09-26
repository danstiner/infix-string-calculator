module StringCalculator
    ( implementations
    , calculate
    ) where

import qualified StringCalculator.AST as AST
import qualified StringCalculator.Postfix as Postfix

implementations :: [(String, String -> Either String Rational)]
implementations =
    [ ("AST", AST.calculate)
    , ("Postfix", Postfix.calculate)
    ]

calculate = Postfix.calculate
