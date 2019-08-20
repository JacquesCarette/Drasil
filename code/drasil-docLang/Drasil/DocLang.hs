module Drasil.DocLang (
  -- DocDecl
  SRSDecl, DocSection(..), ReqrmntSec(..), ReqsSub(..),
  PDSub(..), ProblemDescription(..), SSDSec(..), SSDSub(..), SCSSub(..),
  SolChSpec(..),
  -- DocumentLanguage
  mkDoc, tsymb, tsymb'', tunit, tunit',
  -- DocumentLanguage.Core
  AppndxSec(..), AuxConstntSec(..), DerivationDisplay(..), Emphasis(..),
  OffShelfSolnsSec(..), GSDSec(GSDProg2), GSDSub(UsrChars, SystCons, SysCntxt),
  IntroSec(..), IntroSub(..), LFunc(..), Literature(Doc', Lit,Manual),
  RefSec(..), RefTab(..), StkhldrSec(StkhldrProg2), StkhldrSub(Client, Cstmr),
  TConvention(..), TraceabilitySec(TraceabilityProg), TSIntro(..), TUIntro(..),
  -- DocumentLanguage.Definitions
  Field(..), Fields, InclUnits(IncludeUnits), Verbosity(Verbose), ddefn,
  -- DocumentLanguage.RefHelpers 
  ModelDB, ddRefDB, mdb,
  -- DocumentLanguage.TraceabilityMatrix
  -- Sections.AuxiliaryConstants
  -- Sections.GeneralSystDesc
  -- Sections.Introduction
  -- Sections.ReferenceMaterial
  intro,
  -- Sections.Requirements
  mkInputPropsTable, mkQRTuple, mkQRTupleRef, mkValsSourceTable, 
  -- Sections.SpecificSystemDescription
  auxSpecSent, termDefnF',
  -- Sections.Stakeholders
  -- Sections.TableOfAbbAndAcronyms
  -- Sections.TableOfSymbols
  -- Sections.TableOfUnits
  -- Sections.TraceabilityMandGs
  traceMatStandard,
  -- ExtractDocDesc
  getDocDesc, egetDocDesc, ciGetDocDesc,
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
  DerivationDisplay(..), Emphasis(..), OffShelfSolnsSec(..), GSDSec(GSDProg2),
  GSDSub(UsrChars, SystCons, SysCntxt), IntroSec(..), IntroSub(..), LFunc(..),
  Literature(Doc', Lit,Manual), RefSec(..), RefTab(..), StkhldrSec(StkhldrProg2),
  StkhldrSub(Client, Cstmr), TConvention(..), TraceabilitySec(TraceabilityProg),
  TSIntro(..), TUIntro(..))
import Drasil.DocumentLanguage.Definitions (Field(..), Fields, InclUnits(IncludeUnits),
  Verbosity(Verbose), ddefn)
import Drasil.DocumentLanguage.RefHelpers (ModelDB, ddRefDB, mdb)
--import Drasil.DocumentLanguage.TraceabilityMatrix
--import Drasil.Sections.AuxiliaryConstants
--import Drasil.Sections.GeneralSystDesc
--import Drasil.Sections.Introduction
import Drasil.Sections.ReferenceMaterial (intro)
import Drasil.Sections.Requirements (mkInputPropsTable, mkQRTuple, mkQRTupleRef,
    mkValsSourceTable)
import Drasil.Sections.SpecificSystemDescription (auxSpecSent, termDefnF')
--import Drasil.Sections.Stakeholders
--import Drasil.Sections.TableOfAbbAndAcronyms
--import Drasil.Sections.TableOfSymbols
--import Drasil.Sections.TableOfUnits
import Drasil.Sections.TraceabilityMandGs (traceMatStandard)
import Drasil.ExtractDocDesc (getDocDesc, egetDocDesc, ciGetDocDesc)
import Drasil.TraceTable (generateTraceMap)
-- Commented out modules aren't used - uncomment if this changes
import Drasil.DocumentLanguage.Labels (solutionLabel, characteristicsLabel)
