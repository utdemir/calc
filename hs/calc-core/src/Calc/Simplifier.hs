module Calc.Simplifier (run) where

import Calc.Parser.Types
import Control.Recursion (cata)

run :: Syn -> Syn
run s =
  let s' = cata simplify s
   in if s == s'
        then s'
        else run s'

simplify :: SynF Syn -> Syn
simplify (SynNeg (Fix (SynNum n))) =
  Fix $ SynNum (negate n)
simplify (SynBinOp BinOpAdd (Fix (SynNum lhs)) (Fix (SynNum rhs))) =
  Fix $ SynNum (lhs + rhs)
simplify (SynBinOp BinOpSub (Fix (SynNum lhs)) (Fix (SynNum rhs))) =
  Fix $ SynNum (lhs - rhs)
simplify (SynBinOp BinOpMul (Fix (SynNum lhs)) (Fix (SynNum rhs))) =
  Fix $ SynNum (lhs * rhs)
simplify syn@(SynBinOp BinOpDiv (Fix (SynNum lhs)) (Fix (SynNum rhs))) =
  if rhs /= 0
  then Fix $ SynNum (lhs / rhs)
  else Fix syn
simplify syn@(SynModulo (Fix (SynNum lhs)) (Fix (SynNum rhs))) =
  let lhs' = truncate @_ @Integer lhs
      rhs' = truncate @_ @Integer rhs
   in if (fromIntegral lhs', fromIntegral rhs') == (lhs, rhs)
        then Fix $ SynNum (fromIntegral $ lhs' `mod` rhs')
        else Fix syn
simplify (SynImplMul l r) =
  Fix $ SynBinOp BinOpMul l r
simplify other =
  Fix other
