module Calc.Tokenizer
  ( Located (..),
    Token (..),
    run,
    Tokens,
    tokens,
  )
where

import Calc.Tokenizer.Types
import qualified Data.Char as Char
import Data.List (groupBy)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

newtype Tokens = Tokens {tokens :: NonEmpty Token}

instance IsString Tokens where
  fromString =
    Tokens
      . fromMaybe (pure TokenBlank)
      . NonEmpty.nonEmpty
      . map locatedVal
      . run
      . toText

data CharTy
  = Letter Int
  | Space
  | Symbol
  | Digit
  deriving (Eq)

run :: Text -> [Located Token]
run input =
  input
    & Text.unpack
    & zip [0 ..]
    & map (\(i, c) -> (i, charTy c, c))
    & groupBy ((==) `on` (\(_, ty, _) -> ty))
    & concatMap
      ( \gr ->
          let Just ne = nonEmpty gr
              (start, ty, _) = head ne
              (end, _, _) = last ne
           in case ty of
                -- combine all spaces to a single TokenBlank
                Space ->
                  [Located (start, end) TokenBlank]
                -- combine continuous letters of the same type to a single TokenText
                Letter _ ->
                  [ Located
                      (start, end)
                      (TokenText . toText . toList $ fmap (\(_, _, c) -> c) ne)
                  ]
                Digit ->
                  [ Located
                      (start, end)
                      (TokenNum
                        . fromMaybe (error "invariant violation: cant read digits")
                        . readMaybe @Decimal . toList $ fmap (\(_, _, c) -> c) ne)
                  ]
                -- each symbol becomes their own TokenText
                Symbol ->
                  gr
                    & map (\(i, _, c) -> Located (i, i) (TokenText (Text.pack [c])))
      )
  where
    --
    charTy :: Char -> CharTy
    charTy chr = case Char.generalCategory chr of
      -- letters
      Char.UppercaseLetter -> Letter 0
      Char.LowercaseLetter -> Letter 0
      Char.TitlecaseLetter -> Letter 0
      Char.OtherLetter -> Letter 0
      -- digits
      Char.DecimalNumber -> Digit
      Char.OtherNumber -> Letter 2
      Char.LetterNumber -> Letter 3
      -- symbol
      Char.ConnectorPunctuation -> Symbol
      Char.DashPunctuation -> Symbol
      Char.OpenPunctuation -> Symbol
      Char.ClosePunctuation -> Symbol
      Char.InitialQuote -> Symbol
      Char.FinalQuote -> Symbol
      Char.OtherPunctuation -> Symbol
      Char.MathSymbol -> Symbol
      Char.CurrencySymbol -> Symbol
      Char.OtherSymbol -> Symbol
      -- spaces
      Char.NonSpacingMark -> Space
      Char.SpacingCombiningMark -> Space
      Char.EnclosingMark -> Space
      Char.Space -> Space
      Char.LineSeparator -> Space
      Char.ParagraphSeparator -> Space
      Char.Control -> Space
      Char.Format -> Space
      Char.Surrogate -> Space
      Char.PrivateUse -> Space
      Char.NotAssigned -> Space
      -- TODO modifiers
      Char.ModifierSymbol -> Letter 0
      Char.ModifierLetter -> Letter 0
