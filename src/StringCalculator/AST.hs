{-# LANGUAGE FlexibleContexts #-}

module StringCalculator.AST
    ( calculate
    ) where

import qualified Parser
import Parser (Parser, char, whole)
import           Types

import           Control.Applicative
import           Data.Char

import           Test.QuickCheck.Modifiers

calculate :: String -> Either String Rational
calculate input = Right . eval =<< Parser.run ast input

ast :: Parser AST
ast = pri2 <|> pri1 <|> pri3
    where
        pri1 = multiply <|> divide
        pri2 = add <|> subtract
        pri3 = parenthesized <|> literal <|> negation
        parenthesized = char '(' *> ast <* char ')'
        multiply = Multiply <$> multdivleft <* char '*' <*> multdivright
        divide   = Divide   <$> multdivleft <* char '/' <*> multdivright
        multdivleft = pri3
        multdivright = pri1 <|> pri3
        add      = Add      <$> addsubleft <* char '+' <*> ast
        subtract = Subtract <$> addsubleft <* char '-' <*> ast
        addsubleft = pri1 <|> pri3
        literal = LiteralInteger <$> whole
        negation = Negate <$> (char '-' *> pri3)

eval :: AST -> Rational
eval (LiteralInteger (Positive i)) = fromIntegral i
eval (Subtract left right)         = eval left - eval right
eval (Add left right)              = eval left + eval right
eval (Divide dividend divisor)     = eval dividend / eval divisor
eval (Multiply left right)         = eval left * eval right
eval (Negate ast)                  = -1 * eval ast
