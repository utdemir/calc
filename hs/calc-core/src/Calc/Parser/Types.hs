{-# LANGUAGE TypeSynonymInstances #-}

module Calc.Parser.Types
  ( SynF (..),
    Syn,
    BinOp (..),
    Dimension (..),
    Dimensional (..),

    -- * Recursion schemes helpers
    Recursion.Fix (..),
  )
where

import Calc.Unit.Types
import qualified Control.Recursion as Recursion
import qualified GHC.Show

data BinOp
  = BinOpAdd
  | BinOpSub
  | BinOpMul
  | BinOpDiv
  deriving (Eq)

instance Show BinOp where
  show = \case
    BinOpAdd -> "+"
    BinOpSub -> "-"
    BinOpMul -> "*"
    BinOpDiv -> "/"

data SynF f
  = SynNum Decimal
  | SynNeg f
  | SynBinOp BinOp f f
  | SynImplMul f f
  | --  | SynDimension Dimension
    --  | SynDimensional Dimensional
    --  | SynConv f f
    --  | SynFactorial f
    SynModulo f f
  deriving (Show, Eq, Functor)

type Syn = Recursion.Fix SynF

deriving instance Eq Syn

instance Show Syn where
  show (Recursion.Fix syn) = case syn of
    SynNum d -> show d
    SynNeg d -> parens $ "-" ++ show d
    SynBinOp op l r -> parens $ intercalate " " [show l, show op, show r]
    SynImplMul l r -> parens (show l ++ " " ++ show r)
    SynModulo l r -> parens (show l ++ " mod " ++ show r)
    where
      parens inner = "(" ++ inner ++ ")"
