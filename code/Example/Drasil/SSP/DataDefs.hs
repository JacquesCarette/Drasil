module Drasil.SSP.DataDefs where

import Control.Lens ((^.))
import Prelude hiding (id)

import Language.Drasil
import Drasil.SSP.Units
import Data.Drasil.SI_Units
--import Data.Drasil.Quantities.SolidMechanics

------------------------
--  Data Definitions  --
------------------------

sspDataDefs :: [QDefinition]
sspDataDefs = [sliceWght]

fixmeS :: Sentence
fixmeS = S "FIXME: add description"

--
sliceWght :: QDefinition
sliceWght = fromEqn (wi ^. id) (wi ^. term) (wi ^. symbol) 
  newton slcWgtEqn

slcWgtEqn :: Expr
slcWgtEqn = (Int 0) --FIXME: add the long equation