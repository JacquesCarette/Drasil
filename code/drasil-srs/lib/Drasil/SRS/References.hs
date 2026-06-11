-- | Collects references common to all SRS documents in one list for easy use.
module Drasil.SRS.References (secRefs) where

import Language.Drasil.Document
import Drasil.SRS.Concepts
import Drasil.SRS.DocumentLanguage.Core (getTraceConfigUID)

import Drasil.SRS.Sections.TableOfAbbAndAcronyms (tableAbbAccRef)
import Drasil.SRS.Sections.TableOfSymbols (symbTableRef)
import Drasil.SRS.Sections.TableOfUnits (unitTableRef)
import Drasil.SRS.Sections.TraceabilityMandGs (traceMatAssumpAssump, traceMatAssumpOther, traceMatRefinement)
import Drasil.SRS.Sections.Requirements (reqInputsRef)
import Drasil.SRS.Sections.AuxiliaryConstants (tableOfConstantsRef)
import Drasil.SRS.Sections.SpecificSystemDescription (tInDataCstRef, tOutDataCstRef)

-- | All section references used in creating a Software Requirements Specification (SRS).
secRefs :: [Reference]
secRefs = sectionReferences ++ [tableAbbAccRef, reqInputsRef, symbTableRef,
  unitTableRef, tableOfConstantsRef, tInDataCstRef, tOutDataCstRef]
  ++ map (ref.makeTabRef'.getTraceConfigUID) [traceMatAssumpAssump, -- can this be deleted?
  traceMatAssumpOther, traceMatRefinement]
