module Drasil.SSP.DataCons where 

import Language.Drasil (LabelledContent)

import Drasil.DocLang (inDataConstTbl, outDataConstTbl)

import Drasil.SSP.Unitals (inputsWUncrtn, outputs)

{-input and output tables-}
dataConstraintTable2, dataConstraintTable3 :: LabelledContent
dataConstraintTable2 = inDataConstTbl inputsWUncrtn --FIXME: issue #295
dataConstraintTable3 = outDataConstTbl outputs