{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hedgehog
import Hedgehog.Main
import Calc
import Calc.Tokenizer as Tokenizer

prop_test :: Property
prop_test = property $ do
  doCalc === "Calc"

main :: IO ()
main = defaultMain [checkParallel $$(discover)]
