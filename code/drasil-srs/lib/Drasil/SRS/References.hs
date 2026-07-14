-- | Collects references common to all SRS documents in one list for easy use.
module Drasil.SRS.References (tableReferences) where

import Language.Drasil.Document (Reference)

import Drasil.SRS.Sections.TableOfAbbAndAcronyms (tableAbbAccRef)
import Drasil.SRS.Sections.TableOfSymbols (symbTableRef)
import Drasil.SRS.Sections.TableOfUnits (unitTableRef)
import Drasil.SRS.Sections.Requirements (reqInputsRef)
import Drasil.SRS.Sections.AuxiliaryConstants (tableOfConstantsRef)
import Drasil.SRS.Sections.SpecificSystemDescription (tInDataCstRef, tOutDataCstRef)

-- | All section references used in creating a Software Requirements Specification (SRS).
tableReferences :: [Reference]
tableReferences = [tableAbbAccRef, reqInputsRef, symbTableRef,
  unitTableRef, tableOfConstantsRef, tInDataCstRef, tOutDataCstRef]
