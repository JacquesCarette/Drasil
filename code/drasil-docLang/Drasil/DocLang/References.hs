module Drasil.DocLang.References (secRefs) where

import Drasil.DocLang.SRS
import Drasil.DocumentLanguage.Core (getTraceConfigUID)

import Drasil.Sections.TableOfAbbAndAcronyms (tableAbbAccRef, tableAbbAccLabel)
import Drasil.Sections.TableOfSymbols (symbTableRef)
import Drasil.Sections.TableOfUnits (unitTableRef)
import Drasil.Sections.TraceabilityMandGs (traceMatAssumpAssump, traceMatAssumpOther, traceMatRefinement)
import Drasil.Sections.Requirements (reqInputsRef)
import Drasil.Sections.AuxiliaryConstants (tableOfConstantsRef)
import Drasil.Sections.SpecificSystemDescription (tInDataCstRef, tOutDataCstRef)

import Language.Drasil


secRefs :: [Reference]
secRefs = sectionReferences ++ [tableAbbAccRef, tableAbbAccLabel,
  reqInputsRef, symbTableRef, unitTableRef, tableOfConstantsRef, tInDataCstRef, tOutDataCstRef]
  ++ map (ref.makeTabRef.getTraceConfigUID) [traceMatAssumpAssump, traceMatAssumpOther, traceMatRefinement]