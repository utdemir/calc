{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Main where

import qualified Calc.Lexer as Lexer
import qualified Calc.Parser as Parser
import qualified Calc.Tokenizer as Tokenizer
import System.IO (hFlush)

run :: Text -> Text
run txt =
  let tokens = Tokenizer.run txt & map Tokenizer.locatedVal
      lexemes = Lexer.run tokens
      syns = concatMap Parser.run lexemes
   in mconcat $
        intersperse
          "\n"
          [ show tokens,
            show lexemes,
            show syns
          ]

main :: IO ()
main = do
  putText "> "
  hFlush stdout
  line <- getLine
  putTextLn $ run line
  main
