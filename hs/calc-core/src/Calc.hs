{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Calc
  ( run,
    Result (..),
  )
where

import qualified Calc.Simplifier as Simplifier
import qualified Calc.Lexer as Lexer
import qualified Calc.Parser as Parser
import qualified Calc.Tokenizer as Tokenizer

data Result = Result
  { rInput :: Text,
    rLexemes :: [Lexer.Lexeme],
    rParsed :: Parser.Syn,
    rResult :: Parser.Syn
  }
  deriving (Show)

run :: Text -> [Result]
run txt = do
  let tokens = Tokenizer.run txt & map Tokenizer.locatedVal
  lexemes <- Lexer.run tokens
  parse <- Parser.run lexemes
  let result = Simplifier.run parse
  return (Result txt lexemes parse result)
