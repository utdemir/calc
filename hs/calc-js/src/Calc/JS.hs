{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Calc
import qualified Calc.Lexer.Types as Lexer
import qualified Calc.Parser.Types as Parser
import Data.Aeson
import GHCJS.Foreign.Callback
import GHCJS.Marshal
import GHCJS.Types

#ifdef __GHCJS__
foreign import javascript unsafe "runCalcHs = $1"
  js_set_runCalcHs :: JSVal -> IO ()
#else
js_set_runCalcHs :: JSVal -> IO ()
js_set_runCalcHs = error "not on ghcjs"
#endif

runCalc :: JSVal -> IO JSVal
runCalc str' = do
  str <- fromJSValUnchecked str'
  toJSVal . map resultToValue $ Calc.run str

resultToValue :: Calc.Result -> Value
resultToValue Calc.Result {..} =
  object
    [ "input" .= rInput,
      "lexemes" .= map lexemeToValue rLexemes,
      "parsed" .= synToValue rParsed,
      "result" .= synToValue rResult
    ]
  where
    tagged :: Text -> Value -> Value
    tagged ty val = object ["type" .= ty, "value" .= val]

    lexemeToValue :: Lexer.Lexeme -> Value
    lexemeToValue (Lexer.LexemeNum n) = toJSON (show n :: Text)
    lexemeToValue (Lexer.LexemeText n) = toJSON (show n :: Text)
    lexemeToValue Lexer.LexemeOpenParen = toJSON ("(" :: Text)
    lexemeToValue Lexer.LexemeCloseParen = toJSON (")" :: Text)
    lexemeToValue Lexer.LexemePlus = toJSON ("+" :: Text)
    lexemeToValue Lexer.LexemeMinus = toJSON ("-" :: Text)
    lexemeToValue Lexer.LexemeTimes = toJSON ("*" :: Text)
    lexemeToValue Lexer.LexemeDiv = toJSON ("/" :: Text)
    lexemeToValue Lexer.LexemeMod = toJSON ("mod" :: Text)
    lexemeToValue (Lexer.LexemeUnit t) = unitToValue t

    unitToValue :: Lexer.Unit -> Value
    unitToValue (Lexer.Unit t) = toJSON t

    synToValue :: Parser.Syn -> Value
    synToValue (Parser.Fix (Parser.SynBinOp op l r)) =
      tagged "bin-op" $
        object
          [ "op" .= binOpToName op,
            "lhs" .= synToValue l,
            "rhs" .= synToValue r
          ]
    synToValue (Parser.Fix (Parser.SynNum n)) =
      tagged "num" (toJSON (show n :: Text))
    synToValue (Parser.Fix (Parser.SynNeg n)) =
      tagged "neg" (synToValue n)
    synToValue (Parser.Fix (Parser.SynImplMul l r)) =
      tagged "impl-mul" $
        object
          ["lhs" .= synToValue l, "rhs" .= synToValue r]
    synToValue _ = toJSON ("unknown" :: Text)

    binOpToName :: Parser.BinOp -> Text
    binOpToName Parser.BinOpAdd = "+"
    binOpToName Parser.BinOpSub = "-"
    binOpToName Parser.BinOpMul = "*"
    binOpToName Parser.BinOpDiv = "/"

main :: IO ()
main =
  syncCallback1' runCalc
    <&> jsval
    >>= js_set_runCalcHs
