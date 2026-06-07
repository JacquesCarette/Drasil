-- | Collects references common to all SRS documents in one list for easy use.
module Drasil.SRS.References (secRefs) where

import Language.Drasil.Document
import Drasil.SRS.SRS
import Drasil.SRS.DocumentLanguage.Core (getTraceConfigUID)

import Drasil.Sections.TableOfAbbAndAcronyms (tableAbbAccRef)
import Drasil.Sections.TableOfSymbols (symbTableRef)
import Drasil.Sections.TableOfUnits (unitTableRef)
import Drasil.Sections.TraceabilityMandGs (traceMatAssumpAssump, traceMatAssumpOther, traceMatRefinement)
import Drasil.Sections.Requirements (reqInputsRef)
import Drasil.Sections.AuxiliaryConstants (tableOfConstantsRef)
import Drasil.Sections.SpecificSystemDescription (tInDataCstRef, tOutDataCstRef)

-- | All section references used in creating a Software Requirements Specification (SRS).
secRefs :: [Reference]
secRefs = sectionReferences ++ [tableAbbAccRef, reqInputsRef, symbTableRef,
  unitTableRef, tableOfConstantsRef, tInDataCstRef, tOutDataCstRef]
  ++ map (ref.makeTabRef'.getTraceConfigUID) [traceMatAssumpAssump, -- can this be deleted?
  traceMatAssumpOther, traceMatRefinement]
