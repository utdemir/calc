{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Decimal.Extra
  ( module Data.Decimal,
  )
where

import Data.Decimal
import Language.Haskell.TH.Syntax (Lift)

deriving stock instance Lift Decimal
