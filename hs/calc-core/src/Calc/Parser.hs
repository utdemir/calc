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
                    <$> (term <* token LexemePlus)
                    <*> expr
                ),
          Fix
            <$> ( SynBinOp BinOpSub
                    <$> (term <* token LexemeMinus)
                    <*> expr
                )
        ]

  term <-
    rule $
      asum
        [ factor,
          Fix
            <$> ( SynBinOp BinOpMul
                    <$> (factor <* token LexemeTimes)
                    <*> term
                ),
          Fix
            <$> ( SynBinOp BinOpDiv
                    <$> (factor <* token LexemeDiv)
                    <*> term
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
