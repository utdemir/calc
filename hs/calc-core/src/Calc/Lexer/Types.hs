module Calc.Lexer.Types
  ( Lexeme (..),
  )
where

import Calc.Units

data Lexeme
  = LexemeNum Decimal
  | LexemeText Text
  | LexemeOpenParen
  | LexemeCloseParen
  | LexemePlus
  | LexemeMinus
  | LexemeTimes
  | LexemeDiv
  | LexemeMod
  | LexemeUnit Unit
  deriving (Show, Eq, Ord)
