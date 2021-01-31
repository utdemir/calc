{-# LANGUAGE RecursiveDo #-}

module Calc.Parser where

import Calc.Lexer.Types
import Calc.Parser.Types
import Text.Earley

run :: [Lexeme] -> [Syn]
run lexemes =
  fullParses
    (parser grammar)
    lexemes
    & fst

grammar :: Grammar r (Prod r () Lexeme Syn)
grammar = mdo
  expr <-
    rule $
      asum
        [ term,
          SynBinOp BinOpAdd
            <$> (term <* token LexemePlus)
            <*> term,
          SynBinOp BinOpSub
            <$> (term <* token LexemeMinus)
            <*> term
        ]

  term <-
    rule $
      asum
        [ factor,
          SynBinOp BinOpMul
            <$> (factor <* token LexemeTimes)
            <*> factor,
          SynBinOp BinOpDiv
            <$> (factor <* token LexemeDiv)
            <*> factor
        ]

  factor <-
    rule $
      asum
        [ terminal (\case (LexemeNum i) -> Just (SynNum i); _ -> Nothing),
          token LexemeMinus *> (SynNeg <$> factor),
          token LexemeOpenParen *> expr <* token LexemeCloseParen
        ]

  return expr
