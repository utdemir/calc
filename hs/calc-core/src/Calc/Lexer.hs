module Calc.Lexer
  ( Lexeme (..),
    run,
  )
where

import Calc.Corpus (Corpus)
import qualified Calc.Corpus as Corpus
import Calc.Lexer.Types (Lexeme (..))
import Calc.Tokenizer (Token (..), tokens)

builtins :: Corpus
builtins =
  mconcat
    [ Corpus.singleton LexemePlus (tokens "+"),
      Corpus.singleton LexemeMinus (tokens "-"),
      Corpus.singleton LexemeTimes (tokens "*"),
      Corpus.singleton LexemeDiv (tokens "/")
    ]

corpus :: Corpus
corpus =
  Corpus.default_
    <> builtins

run :: [Token] -> [[Lexeme]]
run = ordNub . go
  where
    go :: [Token] -> [[Lexeme]]
    go [] = [[]]
    go (TokenBlank : xs) = go xs
    go (TokenNum i : xs) = (LexemeNum i :) <$> go xs
    go ts = do
      (lexeme, rest) <-
        Corpus.lookupPrefixes ts corpus
          & toList
      (lexeme :) <$> go rest
