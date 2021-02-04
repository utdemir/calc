module Calc.Evaluator (run) where

import Calc.Parser.Types

run :: Syn -> Either Text Syn
run = eval

eval :: Syn -> Either Text Syn
eval (SynBinOp op lhs rhs) = do
  lhs' <- eval lhs
  rhs' <- eval rhs
  case (lhs', rhs') of
    (SynNum n1, SynNum n2) ->
      Right . SynNum $
        ( case op of
            BinOpAdd -> (+)
            BinOpSub -> (-)
            BinOpMul -> (*)
            BinOpDiv -> (/)
        )
          n1
          n2
    _ -> Left $ "cannot add: " <> show lhs' <> " and " <> show rhs'
eval (SynNum n) = return $ SynNum n
eval (SynNeg i) = do
  i' <- eval i
  case i' of
    SynNum n -> return $ SynNum (negate n)
    _ -> Left $ "cannot negate: " <> show i'
eval other = Left $ "don't know what to do with: " <> show other
