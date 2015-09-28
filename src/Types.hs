module Types (Calculation, AST(..)) where

import           Test.QuickCheck
import           Test.QuickCheck.Modifiers

type Calculation = Either String Rational

data AST = WholeLiteral (Positive Integer)
         | Subtract AST AST
         | Add AST AST
         | Divide AST AST
         | Multiply AST AST
         | Negate AST
  deriving Eq

instance Show AST where
  show (WholeLiteral i) = show i
  show (Subtract a b) = "(" ++ show a ++ "-" ++ show b ++ ")"
  show (Add a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
  show (Divide a b) = "(" ++ show a ++ "/" ++ show b ++ ")"
  show (Multiply a b) = "(" ++ show a ++ "*" ++ show b ++ ")"
  show (Negate ast) = "(-" ++ show ast ++ ")"

instance Arbitrary AST where
  arbitrary = oneof [integer, operator]
    where
      integer = WholeLiteral <$> arbitrary
      operator = oneof
                   [ Subtract <$> arbitrary <*> arbitrary
                   , Add <$> arbitrary <*> arbitrary
                   , Divide <$> arbitrary <*> arbitrary
                   , Multiply <$> arbitrary <*> arbitrary
                   , Negate <$> arbitrary
                   ]
  shrink (Subtract a b) = [a, b]
  shrink (Add a b) = [a, b]
  shrink (Divide a b) = [a, b]
  shrink (Multiply a b) = [a, b]
  shrink (Negate ast) = [ast]
  shrink _ = []
