module Drasil.BinaryStar.DataDefs (dataDefs) where

import Theory.Drasil (DataDefinition)

-- | The hand-written SRS defines DD1 as the gravitational constant G,
-- which in Drasil is handled as a constant (gravitationalConstValue)
-- in Unitals.hs rather than a DataDefinition.
-- The separation distance formula is in TM6 (relPosTM).
dataDefs :: [DataDefinition]
dataDefs = []
