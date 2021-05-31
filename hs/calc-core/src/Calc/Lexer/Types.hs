module Calc.Lexer.Types
  ( Lexeme (..),
    Unit (..),
  )
where

import Calc.Unit.Types

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
  | LexemeUnit UnitCanonicalName
  deriving stock (Show, Eq, Ord, Lift)
