{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Calc where

import qualified Calc.Lexer as Lexer
import qualified Calc.Parser as Parser
import qualified Calc.Tokenizer as Tokenizer
import qualified Calc.Evaluator as Evaluator
import System.IO (hFlush)

run :: Text -> Text
run txt =
  let tokens = Tokenizer.run txt & map Tokenizer.locatedVal
      lexemes = Lexer.run tokens
      syns = concatMap Parser.run lexemes
      res = map Evaluator.run syns
   in mconcat $
        intersperse
          "\n"
          [ show tokens,
            show lexemes,
            show syns,
            show res
          ]

main :: IO ()
main = do
  putText "> "
  hFlush stdout
  line <- getLine
  putTextLn $ run line
  main
