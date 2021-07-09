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
import Drasil.DocumentLanguage.TraceabilityGraph (traceyGraphGetRefs)
import Database.Drasil

import Language.Drasil
import Control.Lens (over, (^.), (.~))


secRefs :: [Reference]
secRefs = sectionReferences ++ [tableAbbAccRef, tableAbbAccLabel,
  reqInputsRef, symbTableRef, unitTableRef, tableOfConstantsRef, tInDataCstRef, tOutDataCstRef]
  ++ map (ref.makeTabRef.getTraceConfigUID) [traceMatAssumpAssump, traceMatAssumpOther, traceMatRefinement]

traceyGraphRefs :: SystemInformation -> SystemInformation
traceyGraphRefs si@SI{_sys = nm} = addRefsToSI si $ traceyGraphGetRefs $ abrv nm

addRefsToSI :: SystemInformation -> [Reference] -> SystemInformation
--addRefsToSI si@SI{_sysinfodb = c} newRefs = si {_sysinfodb = (c {_refTable = idMap (map fst refs ++ newRefs)})}
--addRefsToSI si refs = over (_refTable._sysinfodb) (++ refs) si -- si ^. sysinfodb . refTable
{-addRefsToSI si@SI{_sysinfodb = c} newRefs = si {_sysinfodb = (c {_refTable = idMap (map fst refs ++ newRefs)})}
  where
    refs = c ^. refTable-}
{-addRefsToSI si@SI{_sysinfodb = c} newRefs = si {_sysinfodb = (idMap (map fst refs ++ newRefs) .~ refTable c)}
  where
    refs = c ^. refTable-}