module Calc.Tokenizer.Types
  ( Located (..),
    Token (..),
  )
where

data Token
  = TokenText Text
  | TokenBlank
  | TokenNum Decimal
  deriving (Show, Eq, Ord)

data Located a = Located
  { locatedPos :: (Int, Int),
    locatedVal :: a
  }
  deriving (Show, Functor)
