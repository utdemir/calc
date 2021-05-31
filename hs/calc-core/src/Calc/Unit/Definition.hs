module Calc.Unit.Definition where

import Calc.Unit.Types

data Definition
  = DefinitionBase Quantity
  | DefinitionDerived Text

data UnitDefinition = UnitDefinition
  { udName :: UnitCanonicalName,
    udAlternativeNames :: [Text],
    uAbbbreviations :: [Text],
    udDefinition :: Definition
  }

defs :: [UnitDefinition]
defs =
  [ UnitDefinition
      "metre"
      ["meter"]
      ["m"]
      (DefinitionBase QuantityLength),
    UnitDefinition
      "centimetre"
      ["centimeter"]
      ["cm"]
      (DefinitionDerived "1 metre / 100"),
    UnitDefinition
      "second"
      []
      ["s", "sec"]
      (DefinitionBase QuantityTime),
    UnitDefinition
      "minute"
      []
      ["m", "min"]
      (DefinitionDerived "60 second")
  ]
