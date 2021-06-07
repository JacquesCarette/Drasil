module Drasil.DocLang (
  -- DocDecl
  SRSDecl, DocSection(..), ReqrmntSec(..), ReqsSub(..),
  PDSub(..), ProblemDescription(..), SSDSec(..), SSDSub(..), SCSSub(..),
  SolChSpec(..),
  -- DocumentLanguage
  mkDoc, tsymb, tsymb'', tunit, tunit',
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
  -- Sections.AuxiliaryConstants
  tableOfConstants,
  -- Sections.GeneralSystDesc
  -- Sections.Introduction
  purpDoc,
  -- Sections.ReferenceMaterial
  intro,
  -- Sections.Requirements
  inReq, inTable, mkInputPropsTable, mkQRTuple, mkQRTupleRef, mkValsSourceTable, reqInputsUID,
  -- Sections.SpecificSystemDescription
  auxSpecSent, termDefnF', inDataConstTbl, outDataConstTbl,
  -- Sections.Stakeholders
  -- Sections.TableOfAbbAndAcronyms
  tableAbbAccUID,
  -- Sections.TableOfSymbols
  symbTableRef,
  -- Sections.TableOfUnits
  unitTableRef,
  -- Sections.TraceabilityMandGs
  traceMatStandard,
  -- ExtractDocDesc
  getDocDesc, egetDocDesc,
  -- Tracetable
  generateTraceMap,
 -- Labels
  solutionLabel, characteristicsLabel
) where 

import Drasil.DocDecl (SRSDecl, DocSection(..), ReqrmntSec(..), ReqsSub(..),
  PDSub(..), ProblemDescription(..), SSDSec(..), SSDSub(..), SCSSub(..),
  SolChSpec(..))
import Drasil.DocumentLanguage (mkDoc, tsymb, tsymb'', tunit, tunit')
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
import Drasil.Sections.AuxiliaryConstants (tableOfConstants)
--import Drasil.Sections.GeneralSystDesc
import Drasil.Sections.Introduction (purpDoc)
import Drasil.Sections.ReferenceMaterial (intro)
import Drasil.Sections.Requirements (inReq, inTable, mkInputPropsTable,
  mkQRTuple, mkQRTupleRef, mkValsSourceTable, reqInputsUID)
import Drasil.Sections.SpecificSystemDescription (auxSpecSent, termDefnF', inDataConstTbl, outDataConstTbl)
--import Drasil.Sections.Stakeholders
import Drasil.Sections.TableOfAbbAndAcronyms (tableAbbAccUID)
import Drasil.Sections.TableOfSymbols (symbTableRef)
import Drasil.Sections.TableOfUnits (unitTableRef)
import Drasil.Sections.TraceabilityMandGs (traceMatStandard)
import Drasil.ExtractDocDesc (getDocDesc, egetDocDesc)
import Drasil.TraceTable (generateTraceMap)
-- Commented out modules aren't used - uncomment if this changes
import Drasil.DocumentLanguage.Labels (solutionLabel, characteristicsLabel)
