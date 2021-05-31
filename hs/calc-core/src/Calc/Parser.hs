{-# LANGUAGE RecursiveDo #-}

module Calc.Parser
  ( run,
    Syn,
    SynF,
  )
where

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
          Fix
            <$> ( SynBinOp BinOpAdd
                    <$> (expr <* token LexemePlus)
                    <*> term
                ),
          Fix
            <$> ( SynBinOp BinOpSub
                    <$> (expr <* token LexemeMinus)
                    <*> term
                )
        ]

  term <-
    rule $
      asum
        [ factor,
          Fix
            <$> ( SynBinOp BinOpMul
                    <$> (term <* token LexemeTimes)
                    <*> factor
                ),
          Fix
            <$> ( SynBinOp BinOpDiv
                    <$> (term <* token LexemeDiv)
                    <*> factor
                )
        ]

  factor <-
    rule $
      asum
        [ token LexemeMinus *> (Fix . SynNeg <$> factor),
          token LexemeOpenParen *> expr <* token LexemeCloseParen,
          Fix
            <$> ( SynModulo
                    <$> (factor <* token LexemeMod)
                    <*> factor
                ),
          terminal (\case (LexemeNum i) -> Just (Fix $ SynNum i); _ -> Nothing)
        ]

  return expr
