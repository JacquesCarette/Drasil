-- | Re-export document language types and functions for easy use in other packages.
module Drasil.SRS (
  -- * Document Language
  -- ** SRS
  -- | For generating Software Requirements Specifications.

  -- *** Types
  -- Drasil.DocDecl
  SRSDecl, DocSection(..), ReqrmntSec(..), ReqsSub(..),
  PDSub(..), ProblemDescription(..), SSDSec(..), SSDSub(..), SCSSub(..),
  SolChSpec(..),
  -- Drasil.DocumentLanguage.Core
  AppndxSec(..), AuxConstntSec(..), DerivationDisplay(..), DocDesc, Emphasis(..),
  OffShelfSolnsSec(..), GSDSec(..), GSDSub(UsrChars, SystCons, SysCntxt),
  IntroSec(..), IntroSub(..), LFunc(..), Literature(Doc', Lit,Manual),
  RefSec(..), RefTab(..), StkhldrSec(..), StkhldrSub(Client, Cstmr),
  TConvention(..), TraceabilitySec(TraceabilityProg), TSIntro(..), TUIntro(..),
  -- *** Functions
  -- Drasil.DocumentLanguage
  mkDoc, findAllRefs,
  -- * Subsection Functions
  -- ** Definitions and Models
  -- Drasil.DocumentLanguage.Definitions
  Field(..), Fields, InclUnits(IncludeUnits), Verbosity(..), ddefn,
  -- ** Traceability
  -- Drasil.DocumentLanguage.TraceabilityGraph
  mkGraphInfo, traceyGraphGetRefs,
  -- Drasil.Sections.TraceabilityMandGs
  traceMatStandard, traceMatOtherReq,
  -- Drasil.Tracetable
  generateTraceMap,
  -- ** Auxiliary Constants
  -- Drasil.Sections.AuxiliaryConstants
  tableOfConstants,
  -- ** Introduction
  -- Drasil.Sections.Introduction
  purpDoc,
  -- ** Reference Material
  -- Drasil.Sections.ReferenceMaterial
  intro, emptySectSentPlu, emptySectSentSing,
  -- Drasil.Sections.TableOfSymbols
  tsymb, tsymb'',
  -- Drasil.Sections.TableOfUnits
  unitTableRef, tunit, tunit', tunitNone,
  -- ** Requirements
  -- Drasil.Sections.Requirements
  inReqWTab, outReq, mkInputPropsTable, mkQRTuple, mkQRTupleRef,
  mkValsSourceTable, reqInputsRef, mkMaintainableNFR, mkPortableNFR,
  mkCorrectNFR, mkVerifiableNFR, mkUnderstandableNFR, mkReusableNFR,
  mkSecurityNFR,
  -- ** Specific System Description
  -- Drasil.Sections.SpecificSystemDescription
  auxSpecSent, termDefnF', inDataConstTbl, outDataConstTbl,
  -- * References
  -- Drasil.SRS.References
  secRefs
) where

import Drasil.SRS.DocDecl (SRSDecl, DocSection(..), ReqrmntSec(..), ReqsSub(..),
  PDSub(..), ProblemDescription(..), SSDSec(..), SSDSub(..), SCSSub(..),
  SolChSpec(..))
import Drasil.SRS.DocumentLanguage (mkDoc, findAllRefs)
import Drasil.SRS.DocumentLanguage.Core (AppndxSec(..), AuxConstntSec(..),
  DerivationDisplay(..), DocDesc, Emphasis(..), OffShelfSolnsSec(..), GSDSec(..),
  GSDSub(UsrChars, SystCons, SysCntxt), IntroSec(..), IntroSub(..), LFunc(..),
  Literature(Doc', Lit,Manual), RefSec(..), RefTab(..), StkhldrSec(..),
  StkhldrSub(Client, Cstmr), TConvention(..), TraceabilitySec(TraceabilityProg),
  TSIntro(..), TUIntro(..))
import Drasil.SRS.DocumentLanguage.Definitions (Field(..), Fields, InclUnits(IncludeUnits),
  Verbosity(..), ddefn)
--import Drasil.SRS.DocumentLanguage.TraceabilityMatrix
import Drasil.SRS.DocumentLanguage.TraceabilityGraph (mkGraphInfo, traceyGraphGetRefs)
import Drasil.SRS.Sections.AuxiliaryConstants (tableOfConstants)
--import Drasil.SRS.Sections.GeneralSystDesc
import Drasil.SRS.Sections.Introduction (purpDoc)
import Drasil.SRS.Sections.ReferenceMaterial (intro, emptySectSentPlu, emptySectSentSing)
import Drasil.SRS.Sections.Requirements (mkInputPropsTable,
  mkQRTuple, mkQRTupleRef, mkValsSourceTable, reqInputsRef, mkMaintainableNFR, mkPortableNFR, mkCorrectNFR,
  mkVerifiableNFR, mkUnderstandableNFR, mkReusableNFR, mkSecurityNFR, inReqWTab, outReq)
import Drasil.SRS.Sections.SpecificSystemDescription (auxSpecSent, termDefnF', inDataConstTbl, outDataConstTbl)
--import Drasil.SRS.Sections.Stakeholders
--import Drasil.SRS.Sections.TableOfAbbAndAcronyms
import Drasil.SRS.Sections.TableOfSymbols (tsymb, tsymb'')
import Drasil.SRS.Sections.TableOfUnits (unitTableRef, tunit, tunit',tunitNone)
import Drasil.SRS.Sections.TraceabilityMandGs (traceMatStandard, traceMatOtherReq)
import Drasil.SRS.TraceTable (generateTraceMap)
import Drasil.SRS.References (secRefs)
-- Commented out modules aren't used - uncomment if this changes
