module Types
	( AST (..)
	, Calculation
	) where

import Test.QuickCheck.Modifiers

type Calculation = Either String Integer

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
