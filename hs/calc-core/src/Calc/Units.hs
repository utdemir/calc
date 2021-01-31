module Calc.Units where

data Quantity
  = QuantityLength
  deriving (Show, Eq, Ord)

data Unit
  = Unit Quantity Text
  deriving (Show, Eq, Ord)

{-
data UnitDefinition = UnitDefinition
  { udQuantity :: Quantity,
    udName :: NonEmpty Name
  }

data UnitConversion = UnitConversion
  { ucFrom :: Unit,
    ucTo :: Unit,
    ucConvert :: Rational -> Rational
  }

data Fact
  = FactUnitDefinition UnitDefinition
  | FactUnitConversion UnitConversion

facts :: [Fact]
facts =
  [ FactUnitDefinition $
      UnitDefinition
        QuantityLength
        ("metre" :| ["meter", "m"]),
    FactUnitDefinition $
      UnitDefinition
        QuantityLength
        ("centimeters" :| ["centimetres", "cm"]),
    FactUnitConversion $
      UnitConversion
        (Unit "metre")
        (Unit "centimeters")
        (* 100),
    FactUnitConversion $
      UnitConversion
        (Unit "centimeters")
        (Unit "meters")
        (/ 100)
  ]
-}
