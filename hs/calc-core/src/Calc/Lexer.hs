module Calc.Lexer
  ( Lexeme (..),
    run,
  )
where

import Calc.Corpus (Corpus)
import qualified Calc.Corpus as Corpus
import Calc.Lexer.Types (Lexeme (..))
import Calc.Tokenizer (Token (..))

builtins :: Corpus
builtins =
  mconcat
    [ Corpus.singleton LexemePlus (TokenText "+" :| []),
      Corpus.singleton LexemeMinus (TokenText "-" :| []),
      Corpus.singleton LexemeTimes (TokenText "*" :| []),
      Corpus.singleton LexemeDiv (TokenText "/" :| []),
      Corpus.singleton LexemeMod (TokenText "mod" :| []),
      Corpus.singleton LexemeMod (TokenText "%" :| []),
      Corpus.singleton LexemeOpenParen (TokenText "(" :| []),
      Corpus.singleton LexemeCloseParen (TokenText ")" :| [])
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
    go (TokenNum i : TokenText "." : TokenNum f : xs) =
      let d =
            readMaybe @Decimal (show i ++ "." ++ show f)
              & fromMaybe (error "invariant violation: can not read decimal")
       in (LexemeNum d :) <$> go xs
    go (TokenNum i : xs) = (LexemeNum i :) <$> go xs
    go ts = do
      (lexeme, rest) <-
        Corpus.lookupPrefixes ts corpus
          & toList
      (lexeme :) <$> go rest
