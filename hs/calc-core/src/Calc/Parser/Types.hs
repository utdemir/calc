{-# LANGUAGE TypeSynonymInstances #-}

module Calc.Parser.Types
  ( SynF (..),
    Syn,
    BinOp (..),
    Unit (..),
    Quantity (..),
    Dimension (..),
    Dimensional (..),

    -- * Recursion schemes helpers
    Recursion.Fix (..),
  )
where

import Calc.Units
import qualified Control.Recursion as Recursion
import qualified GHC.Show

data Dimension
  = DimensionOne
  | DimensionSimple Unit
  | DimensionMul Dimension Dimension
  | DimensionDiv Dimension Dimension
  | DimensionPow Dimension Integer
  deriving (Show, Eq)

data Dimensional
  = Dimensional Decimal Dimension
  deriving (Show, Eq)

data BinOp
  = BinOpAdd
  | BinOpSub
  | BinOpMul
  | BinOpDiv
  deriving (Show, Eq)

data SynF f
  = SynNum Decimal
  | SynNeg f
  | SynBinOp BinOp f f
  | SynImplMul f f
  | SynDimension Dimension
  | SynDimensional Dimensional
  | SynConv f f
  | SynFactorial f
  | SynModulo f f
  deriving (Show, Eq, Functor)

type Syn = Recursion.Fix SynF

instance Eq Syn where
  Recursion.Fix s1 == Recursion.Fix s2 = s1 == s2

instance Show Syn where
  show (Recursion.Fix s1) = "(" ++ show s1 ++ ")"
