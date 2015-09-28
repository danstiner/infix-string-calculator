{-# LANGUAGE TemplateHaskell #-}

module StringCalculator.Prefix (calculate, tests) where

import           Parser                               hiding (tests)
import           Types

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.DeriveTH
import           Data.Word
import           Test.Fluent
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.Framework.TH
import           Test.QuickCheck
import           Test.QuickCheck.Modifiers
import           Test.QuickCheck.Property             as Property

data Token = WholeToken (Positive Integer)
           | Minus
           | Plus
           | LeftParen
           | RightParen
           | Slash
           | Asterick
           | NegateTok
  deriving Eq

instance Show Token where
  show (WholeToken (Positive i)) = show i
  show Minus = "-"
  show Plus = "+"
  show LeftParen = "("
  show RightParen = ")"
  show Slash = "/"
  show Asterick = "*"
  show NegateTok = "-"

newtype Prefix = Prefix [Token]

$(derive makeArbitrary ''Token)

calculate :: String -> Calculation
calculate = eval <=< toPrefix <=< lexString

lexString :: String -> Either ParseError [Token]
lexString = parse (lexer <* eof)

lexer :: Parser [Token]
lexer = (++) <$> many negation <*> (concat <$> some token)

token :: Parser [Token]
token = wholeTok' <|> opThenNegation <|> operator' <|> rightParen'
  where
    wholeTok' = (: []) <$> wholeTok
    operator' = (: []) <$> operator
    rightParen' = (: []) <$> rightParen

opThenNegation :: Parser [Token]
opThenNegation = (:) <$> (operator <|> leftParen) <*> many negation

negation :: Parser Token
negation = char '-' *> pure NegateTok

operator :: Parser Token
operator = minus <|> plus <|> divide <|> times

wholeTok, minus, plus, leftParen, rightParen, divide, times :: Parser Token
wholeTok = WholeToken <$> whole

minus = char '-' *> pure Minus

plus = char '+' *> pure Plus

leftParen = char '(' *> pure LeftParen

rightParen = char ')' *> pure RightParen

divide = char '/' *> pure Slash

times = char '*' *> pure Asterick

toPrefix :: [Token] -> Either String Prefix
toPrefix tokens = toPrefix' tokens [] []
  where
    toPrefix' :: [Token] -> [Token] -> [Token] -> Either String Prefix
    toPrefix' (WholeToken (Positive i):tokens) opstack partial = toPrefix' tokens opstack
                                                                   (WholeToken (Positive i) : partial)
    toPrefix' (NegateTok:tokens) opstack partial = toPrefix' tokens (NegateTok : opstack) partial
    toPrefix' (Plus:tokens) opstack partial = toPrefix' tokens (Plus : opstack) partial
    toPrefix' [] [] partial = Right (Prefix partial)
    toPrefix' [] (NegateTok:opstack) partial = toPrefix' [] opstack (NegateTok : partial)
    toPrefix' tokens opstack partial = Left
                                         ("Unexpected infix, tokens: " ++
                                          show tokens ++
                                          " opstack: " ++
                                          show opstack ++
                                          " partial: " ++
                                          show partial)

eval :: Prefix -> Calculation
eval (Prefix tokens) = eval' tokens
  where
    eval' [WholeToken (Positive i)] = Right (fromIntegral i)
    eval' (NegateTok:tokens) = ((-1) *) <$> eval' tokens
    eval' ts = Left ("Unexpected: " ++ show ts)

prop_tokenize_an_integer :: Positive Integer -> Property.Result
prop_tokenize_an_integer (Positive i) = Right [WholeToken (Positive i)] `shouldBe` lexString
                                                                                     (show i)

prop_tokenize_top_level_negation :: Int -> Property.Result
prop_tokenize_top_level_negation count =
  lexString (tokensToString tokens) `shouldBe` Right tokens
  where
    tokens = replicate count NegateTok ++ [one]
    one = WholeToken (Positive 1)

prop_tokenize_ast :: AST -> Property.Result
prop_tokenize_ast ast =
  lexString (tokensToString tokens) `shouldBe` Right tokens
  where
    tokens = astToTokens ast

prop_calculate_a_whole :: Positive Integer -> Property.Result
prop_calculate_a_whole (Positive i) = calculate (show i) `shouldBe` Right (fromIntegral i)

prop_calculate_a_negated_whole :: Positive Integer -> Property.Result
prop_calculate_a_negated_whole (Positive i) = calculate ("-" ++ show i) `shouldBe` Right
                                                                                     (-1 * fromIntegral
                                                                                             i)

astToTokens :: AST -> [Token]
astToTokens (WholeLiteral p) = [WholeToken p]
astToTokens (Subtract a b) = astToTokens a ++ [Minus] ++ astToTokens b
astToTokens (Add a b) = astToTokens a ++ [Plus] ++ astToTokens b
astToTokens (Divide a b) = astToTokens a ++ [Slash] ++ astToTokens b
astToTokens (Multiply a b) = astToTokens a ++ [Asterick] ++ astToTokens b
astToTokens (Negate ast) = NegateTok : astToTokens ast

tokensToString :: [Token] -> String
tokensToString = concatMap show

tests = $(testGroupGenerator)
