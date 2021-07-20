module Drasil.DocLang.References (secRefs, fillTraceSI, fillcdbSRS, fillReferences) where

import Drasil.DocLang.SRS
import Drasil.DocumentLanguage.Core -- (getTraceConfigUID)
import Drasil.DocDecl (SRSDecl, mkDocDesc)
import Drasil.TraceTable (generateTraceMap)

import Drasil.Sections.TableOfAbbAndAcronyms (tableAbbAccRef)
import Drasil.Sections.TableOfSymbols (symbTableRef)
import Drasil.Sections.TableOfUnits (unitTableRef)
import Drasil.Sections.TraceabilityMandGs (traceMatAssumpAssump, traceMatAssumpOther, traceMatRefinement, traceMatStandard)
import Drasil.Sections.Requirements (reqInputsRef)
import Drasil.Sections.AuxiliaryConstants (tableOfConstantsRef)
import Drasil.Sections.SpecificSystemDescription (tInDataCstRef, tOutDataCstRef)
import Drasil.DocumentLanguage.TraceabilityGraph (traceyGraphGetRefs)

import Language.Drasil
import Database.Drasil

import Data.Drasil.Concepts.Computation (compcon, compcon')
import Data.Drasil.Concepts.Documentation (doccon, doccon')
import Data.Drasil.Concepts.Education (educon)
import Data.Drasil.Concepts.Math (mathcon, mathcon')
import Data.Drasil.Concepts.PhysicalProperties (physicalcon)
import Data.Drasil.Concepts.Physics (physicCon, physicCon')
import Data.Drasil.Concepts.Software (softwarecon)
import Data.Drasil.Concepts.SolidMechanics (solidcon)
import Data.Drasil.Concepts.Thermodynamics (thermocon)

import Data.List (nub, sortOn)
import qualified Data.Map as Map
import Control.Lens (set, (^.))


secRefs :: [Reference]
secRefs = sectionReferences ++ [tableAbbAccRef, reqInputsRef, symbTableRef,
  unitTableRef, tableOfConstantsRef, tInDataCstRef, tOutDataCstRef]
  ++ map (ref.makeTabRef.getTraceConfigUID) [traceMatAssumpAssump,
  traceMatAssumpOther, traceMatRefinement]

{-cdbSRS :: (Quantity q, MayHaveUnit q, Idea t, Concept c, IsUnit u) =>
  [q] -> [t] -> [c] -> [u] -> [DataDefinition] -> [InstanceModel] ->
  [GenDefn] -> [TheoryModel] -> [ConceptInstance] -> [Section] ->
  [LabelledContent] -> [Reference] -> ChunkDB
cdbSRS s t c u d ins gd tm ci sect lc r = cdb s t c u d ins gd tm ci sect lc r-}

-- assuming minimal chunkdb, fill in for rest of system information.
fillcdbSRS :: SRSDecl -> SystemInformation -> SystemInformation
fillcdbSRS srsDec si = fillTraceSI srsDec $ fillReferences $ fillConcepts si


fillConcepts :: SystemInformation -> SystemInformation
fillConcepts si = si2
  where
    si2 = set sysinfodb chkdb2 si
    chkdb = si ^. sysinfodb
    tmtbl = termTable chkdb
    chkdb2 = chkdb{termTable = termMap $ nub (map nw doccon ++ map nw doccon' ++ map nw softwarecon ++ map nw physicCon ++ map nw physicCon' ++ map nw physicalcon ++ map nw educon ++ map nw mathcon ++ map nw mathcon' ++ map nw compcon ++ map nw compcon' ++ map nw solidcon ++ map nw thermocon ++ (map (fst.snd) $ Map.assocs tmtbl))}

-- Takes in existing information from the Chunk database to construct a database of references.
fillReferences :: SystemInformation -> SystemInformation
fillReferences si = si2
  where
    si2 = set sysinfodb chkdb2 si
    chkdb = si ^. sysinfodb
    rfdb = refdb si
    chkdb2 = set refTable (idMap $ nub (map (ref.makeTabRef.getTraceConfigUID) (traceMatStandard si) ++ traceyGraphGetRefs (si ^. folderPath) ++ secRefs ++ map ref cites ++ map ref conins ++ map ref ddefs ++ map ref gdefs ++ map ref imods ++ map ref tmods ++ map ref concIns ++ map ref secs ++ map ref lblCon ++ refs)) chkdb 
    ddefs   = map (fst.snd) $ Map.assocs $ chkdb ^. dataDefnTable
    gdefs   = map (fst.snd) $ Map.assocs $ chkdb ^. gendefTable
    imods   = map (fst.snd) $ Map.assocs $ chkdb ^. insmodelTable
    tmods   = map (fst.snd) $ Map.assocs $ chkdb ^. theoryModelTable
    concIns = map (fst.snd) $ Map.assocs $ chkdb ^. conceptinsTable
    secs    = map (fst.snd) $ Map.assocs $ chkdb ^. sectionTable
    lblCon  = map (fst.snd) $ Map.assocs $ chkdb ^. labelledcontentTable
    refs    = map (fst.snd) $ Map.assocs $ chkdb ^. refTable
    cites   = map (fst.snd) $ Map.assocs $ rfdb  ^. citationDB
    conins  = map (fst.snd) $ Map.assocs $ rfdb  ^. conceptDB

-- | Helper for filling in the traceability matrix and graph information into the system.
fillTraceSI :: SRSDecl -> SystemInformation -> SystemInformation
fillTraceSI dd si = fillTraceMaps l $ fillReqs l si
  where
    l = mkDocDesc si dd

-- | Fills in the traceabiliy matrix and graphs section of the system information using the document description.
fillTraceMaps :: DocDesc -> SystemInformation -> SystemInformation
fillTraceMaps dd si@SI{_sysinfodb = db} = si {_sysinfodb =
  set refbyTable (generateRefbyMap tdb) $ set traceTable tdb db} where
  tdb = generateTraceMap dd

-- | Fills in the requirements section of the system information using the document description.
fillReqs :: DocDesc -> SystemInformation -> SystemInformation
fillReqs [] si = si
fillReqs (ReqrmntSec (ReqsProg x):_) si@SI{_sysinfodb = db} = genReqs x
  where
    genReqs [] = si
    genReqs (FReqsSub c _:_) = si {_sysinfodb = set conceptinsTable newCI db} where
        newCI = idMap $ nub $ c ++ map fst (sortOn snd $ map snd $ Map.toList $ db ^. conceptinsTable)
    genReqs (_:xs) = genReqs xs
fillReqs (_:xs) si = fillReqs xs si