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
      "(SynNum 4)"
    ),
    ( "2+2*2",
      "(SynNum 6)"
    ),
    ( "5 mod 2",
      "(SynNum 1)"
    ),
    ( "5.2 mod 2",
      "(SynModulo (SynNum 5.2) (SynNum 2))"
    ),
    ( "1 + 1 mod 1",
      "(SynNum 1)"
    )
  ]
    & map
      ( \(input, expected) ->
          (fromString . toString $ expected, mkTest input expected)
      )
    & Group "examples"

main :: IO ()
main = defaultMain $ [checkParallel examples]
