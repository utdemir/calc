module Calc.Parser.Types
  ( Syn (..),
    BinOp (..),
    Unit (..),
    Quantity (..),
    Dimension (..),
    Dimensional (..),
  )
where

import Calc.Units

data Dimension
  = DimensionOne
  | DimensionSimple Unit
  | DimensionMul Dimension Dimension
  | DimensionDiv Dimension Dimension
  | DimensionPow Dimension Integer
  deriving (Show)

data Dimensional
  = Dimensional Decimal Dimension
  deriving (Show)

data BinOp
  = BinOpAdd
  | BinOpSub
  | BinOpMul
  | BinOpDiv
  deriving (Show)

data Syn
  = SynNum Decimal
  | SynNeg Syn
  | SynBinOp BinOp Syn Syn
  | SynImplMul Syn Syn
  | SynDimension Dimension
  | SynDimensional Dimensional
  | SynConv Syn Syn
  | SynFactorial Syn
  deriving (Show)
