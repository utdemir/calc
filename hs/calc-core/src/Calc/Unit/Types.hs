module Calc.Unit.Types where

newtype UnitCanonicalName
  = UnitCanonicalName Text
  deriving stock (Eq, Ord, Show, Lift)
  deriving newtype (IsString, Hashable)

data Unit = Unit
  { uName :: UnitCanonicalName,
    uAlternativeNames :: [Text],
    uAbbreviations :: [Text],
    uDimension :: Dimension
  }
  deriving stock (Show, Eq)

newtype UnitOf (a :: Quantity) = UnitOf UnitCanonicalName
  deriving stock (Eq, Show)
  deriving newtype (Hashable)

data Quantity
  = QuantityLength
  | QuantityTime
  | QuantityMass
  | QuantityElectricCurrent
  | QuantityThermodynamicTemperature
  | QuantityAmountOfSubstance
  | QuantityLuminousIntensity
  deriving stock (Show, Eq)

data Dimension
  = Dimension
      (UnitOf 'QuantityLength, Int)
      (UnitOf 'QuantityTime, Int)
      (UnitOf 'QuantityMass, Int)
      (UnitOf 'QuantityElectricCurrent, Int)
      (UnitOf 'QuantityThermodynamicTemperature, Int)
      (UnitOf 'QuantityAmountOfSubstance, Int)
      (UnitOf 'QuantityLuminousIntensity, Int)
  deriving stock (Eq, Show)

data Dimensional
  = Dimensional Decimal Dimension
  deriving stock (Show, Eq)
