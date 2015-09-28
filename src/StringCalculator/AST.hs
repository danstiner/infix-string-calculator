module StringCalculator.AST (calculate) where

import           Parser                    (Parser, char, eof, parse, whole)
import qualified Parser
import           Types

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Either.Combinators
import           Test.QuickCheck.Modifiers

calculate :: String -> Calculation
calculate = fmap eval . parse parser

parser :: Parser AST
parser = ast <* eof
  where
    ast = addsub <|> multdiv <|> unambiguous
    multdiv = multiply <|> divide
    addsub = add <|> subtract
    unambiguous = parenthesized <|> literal <|> negation
    parenthesized = char '(' *> ast <* char ')'
    multiply = Multiply <$> multdivleft <* char '*' <*> multdivright
    divide = Divide <$> multdivleft <* char '/' <*> multdivright
    multdivleft = unambiguous
    multdivright = multdiv <|> unambiguous
    add = Add <$> addsubleft <* char '+' <*> ast
    subtract = Subtract <$> addsubleft <* char '-' <*> ast
    addsubleft = multdiv <|> unambiguous
    literal = WholeLiteral <$> whole
    negation = Negate <$> (char '-' *> unambiguous)

eval :: AST -> Rational
eval (WholeLiteral (Positive i)) = fromIntegral i
eval (Subtract left right) = eval left - eval right
eval (Add left right) = eval left + eval right
eval (Divide dividend divisor) = eval dividend / eval divisor
eval (Multiply left right) = eval left * eval right
eval (Negate ast) = -1 * eval ast
