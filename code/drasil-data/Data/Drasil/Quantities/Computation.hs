module Data.Drasil.Quantities.Computation where

import Language.Drasil
import Language.Drasil.ShortHands

import Data.Drasil.Concepts.Computation as CC (char, integer, nat, real, string)

char, integer, nat, real, string :: DefinedQuantityDict

char    = dqd' CC.char     (const $ Atomic "char") Real Nothing
integer = dqd' CC.integer  (const $ cZ)            Real Nothing
nat     = dqd' CC.nat      (const $ cN)            Real Nothing
real    = dqd' CC.real     (const $ cR)            Real Nothing
string  = dqd' CC.string   (const $ cS)            Real Nothing
