module StringCalculator.AST
    ( calculate
    ) where

import           Parser                    (Parser, parse, char, whole, eof)
import qualified Parser
import           Types

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Either.Combinators
import           Test.QuickCheck.Modifiers

data AST =
    LiteralInteger (Positive Integer)
    | Subtract AST AST
    | Add AST AST
    | Divide AST AST
    | Multiply AST AST
    | Negate AST
    deriving (Eq)

instance Show AST where
    show (LiteralInteger (Positive i)) = show i
    show (Subtract a b) = "(" ++ show a ++ "-" ++ show b ++ ")"
    show (Add a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
    show (Divide a b) = "(" ++ show a ++ "/" ++ show b ++ ")"
    show (Multiply a b) = "(" ++ show a ++ "*" ++ show b ++ ")"
    show (Negate ast) = "(-" ++ show ast ++ ")"

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
