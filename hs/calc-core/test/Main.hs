{-# LANGUAGE TemplateHaskell #-}

module Main where

import Calc
import Hedgehog
import Hedgehog.Main

mkTest :: Text -> Text -> Property
mkTest input expected = withTests 1 . property $ do
  annotateShow input
  let results = Calc.run input
  annotateShow results
  let actual =
        viaNonEmpty (show . rResult . head) results
          & fromMaybe ""
  actual === expected

examples :: Group
examples =
  [ ( "2+2",
      "4"
    ),
    ( "2+2*2",
      "6"
    ),
    ( "5 mod 2",
      "1"
    ),
    ( "5.2 mod 2",
      "(5.2 mod 2)"
    ),
    ( "1 + 1 mod 1",
      "1"
    ),
    ( "1 - 1 - 1",
      "-1"
    )
  ]
    & map
      ( \(input, expected) ->
          (fromString . toString $ "'" <> input <> "'", mkTest input expected)
      )
    & Group "examples"

main :: IO ()
main = defaultMain $ [checkParallel examples]
