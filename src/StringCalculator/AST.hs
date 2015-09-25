{-# LANGUAGE FlexibleContexts #-}

module StringCalculator.AST
    ( calculate
    ) where

import           Parser                    (Parser, char, whole)
import qualified Parser
import           Types

import           Control.Applicative
import           Data.Char

import           Test.QuickCheck.Modifiers

calculate :: String -> Either String Rational
calculate input = Right . eval =<< Parser.run ast input

ast :: Parser AST
ast = addsub <|> multdiv <|> unambiguous
    where
        multdiv = multiply <|> divide
        addsub = add <|> subtract
        unambiguous = parenthesized <|> literal <|> negation
        parenthesized = char '(' *> ast <* char ')'
        multiply = Multiply <$> multdivleft <* char '*' <*> multdivright
        divide   = Divide   <$> multdivleft <* char '/' <*> multdivright
        multdivleft = unambiguous
        multdivright = multdiv <|> unambiguous
        add      = Add      <$> addsubleft <* char '+' <*> ast
        subtract = Subtract <$> addsubleft <* char '-' <*> ast
        addsubleft = multdiv <|> unambiguous
        literal = LiteralInteger <$> whole
        negation = Negate <$> (char '-' *> unambiguous)

eval :: AST -> Rational
eval (LiteralInteger (Positive i)) = fromIntegral i
eval (Subtract left right)         = eval left - eval right
eval (Add left right)              = eval left + eval right
eval (Divide dividend divisor)     = eval dividend / eval divisor
eval (Multiply left right)         = eval left * eval right
eval (Negate ast)                  = -1 * eval ast
