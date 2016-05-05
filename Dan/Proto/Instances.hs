{-# OPTIONS_GHC -fno-warn-orphans #-}
-- Collect some (by design!) 'orphan' instances that can't be
-- meaningfully defined any earlier
module Instances where

import Unicode(Render(..))
import Symbol(Symbol(..))
import Format

import qualified PrintTeX as T
import qualified PrintHTML as H

-- as there is no PrintPlain, do this here; a hack in cases.
symbol :: Symbol -> String
symbol (Atomic s)  = s
symbol (Special s) = render Plain s
symbol (Concat sl) = foldr (++) "" $ map symbol sl
symbol (Corners _ _ _ _ _) = error "2D printing not supported in Plain mode"
symbol (Atop _ _)       = error "2D printing not support in Plain mode"

instance Render Symbol where
  render TeX   = T.symbol
  render Plain =   symbol
  render HTML  = H.symbol
