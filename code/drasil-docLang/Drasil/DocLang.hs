module Drasil.DocLang (
  -- DocDecl
  SRSDecl, DocSection(..), ReqrmntSec(..), ReqsSub(..),
  PDSub(..), ProblemDescription(..), SSDSec(..), SSDSub(..), SCSSub(..),
  SolChSpec(..),
  -- DocumentLanguage
  mkDoc, fillTraceSI, fillcdbSRS,
  -- DocumentLanguage.Core
  AppndxSec(..), AuxConstntSec(..), DerivationDisplay(..), Emphasis(..),
  OffShelfSolnsSec(..), GSDSec(..), GSDSub(UsrChars, SystCons, SysCntxt),
  IntroSec(..), IntroSub(..), LFunc(..), Literature(Doc', Lit,Manual),
  RefSec(..), RefTab(..), StkhldrSec(..), StkhldrSub(Client, Cstmr),
  TConvention(..), TraceabilitySec(TraceabilityProg), TSIntro(..), TUIntro(..),
  getTraceConfigUID,
  -- DocumentLanguage.Definitions
  Field(..), Fields, InclUnits(IncludeUnits), Verbosity(..), ddefn,
  -- DocumentLanguage.RefHelpers 
  ModelDB, ddRefDB, mdb,
  -- DocumentLanguage.TraceabilityMatrix
  -- DocumentLanguage.TraceabilityGraph
  mkGraphInfo, traceyGraphGetRefs,
  -- Sections.AuxiliaryConstants
  tableOfConstants,
  -- Sections.GeneralSystDesc
  -- Sections.Introduction
  purpDoc,
  -- Sections.ReferenceMaterial
  intro,
  -- Sections.Requirements
  inReq, inTable, mkInputPropsTable, mkQRTuple, mkQRTupleRef, mkValsSourceTable, reqInputsRef,
  -- Sections.SpecificSystemDescription
  auxSpecSent, termDefnF', inDataConstTbl, outDataConstTbl,
  -- Sections.Stakeholders
  -- Sections.TableOfAbbAndAcronyms
  tableAbbAccRef,
  -- Sections.TableOfSymbols
  symbTableRef, tsymb, tsymb'',
  -- Sections.TableOfUnits
  unitTableRef, tunit, tunit',
  -- Sections.TraceabilityMandGs
  traceMatStandard, traceMatOtherReq,
  -- ExtractDocDesc
  getDocDesc, egetDocDesc,
  -- Tracetable
  generateTraceMap,
  -- References
  secRefs
) where 

import Drasil.DocDecl (SRSDecl, DocSection(..), ReqrmntSec(..), ReqsSub(..),
  PDSub(..), ProblemDescription(..), SSDSec(..), SSDSub(..), SCSSub(..),
  SolChSpec(..))
import Drasil.DocumentLanguage (mkDoc, fillTraceSI, fillcdbSRS)
import Drasil.DocumentLanguage.Core (AppndxSec(..), AuxConstntSec(..),
  DerivationDisplay(..), Emphasis(..), OffShelfSolnsSec(..), GSDSec(..),
  GSDSub(UsrChars, SystCons, SysCntxt), IntroSec(..), IntroSub(..), LFunc(..),
  Literature(Doc', Lit,Manual), RefSec(..), RefTab(..), StkhldrSec(..),
  StkhldrSub(Client, Cstmr), TConvention(..), TraceabilitySec(TraceabilityProg),
  TSIntro(..), TUIntro(..), getTraceConfigUID)
import Drasil.DocumentLanguage.Definitions (Field(..), Fields, InclUnits(IncludeUnits),
  Verbosity(..), ddefn)
import Drasil.DocumentLanguage.RefHelpers (ModelDB, ddRefDB, mdb)
--import Drasil.DocumentLanguage.TraceabilityMatrix
import Drasil.DocumentLanguage.TraceabilityGraph (mkGraphInfo, traceyGraphGetRefs)
import Drasil.Sections.AuxiliaryConstants (tableOfConstants)
--import Drasil.Sections.GeneralSystDesc
import Drasil.Sections.Introduction (purpDoc)
import Drasil.Sections.ReferenceMaterial (intro)
import Drasil.Sections.Requirements (inReq, inTable, mkInputPropsTable,
  mkQRTuple, mkQRTupleRef, mkValsSourceTable, reqInputsRef)
import Drasil.Sections.SpecificSystemDescription (auxSpecSent, termDefnF', inDataConstTbl, outDataConstTbl)
--import Drasil.Sections.Stakeholders
import Drasil.Sections.TableOfAbbAndAcronyms (tableAbbAccRef)
import Drasil.Sections.TableOfSymbols (symbTableRef, tsymb, tsymb'')
import Drasil.Sections.TableOfUnits (unitTableRef, tunit, tunit')
import Drasil.Sections.TraceabilityMandGs (traceMatStandard, traceMatOtherReq)
import Drasil.ExtractDocDesc (getDocDesc, egetDocDesc)
import Drasil.TraceTable (generateTraceMap)
-- Commented out modules aren't used - uncomment if this changes
import Drasil.DocLang.References (secRefs)
