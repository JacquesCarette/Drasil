-- | Re-export document language types and functions for easy use in other packages.
module Drasil.SRS (
  module Drasil.SRS.SmithEtAlSRS,
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
  PurposeDescription(..),
  -- *** Functions
  -- Drasil.DocumentLanguage
  mkDoc,
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
  -- ** Reference Material
  -- Drasil.Sections.ReferenceMaterial
  intro, emptySectSentPlu, emptySectSentSing,
  -- Drasil.Sections.TableOfSymbols
  tsymb, tsymb'',
  -- Drasil.Sections.TableOfUnits
  unitTableRef, tunit, tunit', tunitNone,
  -- ** Requirements
  -- Drasil.Sections.Requirements
  inReqWTab, mkInputPropsTable, mkQRTuple, mkQRTupleRef,
  mkValsSourceTable, reqInputsRef, mkMaintainableNFR, mkPortableNFR,
  mkCorrectNFR, mkVerifiableNFR, mkUnderstandableNFR, mkReusableNFR,
  mkSecurityNFR,
  -- ** Specific System Description
  -- Drasil.Sections.SpecificSystemDescription
  auxSpecSent, termDefnF', inDataConstTbl, outDataConstTbl,
) where

import Drasil.SRS.DocDecl (SRSDecl, DocSection(..), ReqrmntSec(..), ReqsSub(..),
  PDSub(..), ProblemDescription(..), SSDSec(..), SSDSub(..), SCSSub(..),
  SolChSpec(..))
import Drasil.SRS.DocumentLanguage (mkDoc)
import Drasil.SRS.DocumentLanguage.Core (AppndxSec(..), AuxConstntSec(..),
  DerivationDisplay(..), DocDesc, Emphasis(..), OffShelfSolnsSec(..), GSDSec(..),
  GSDSub(UsrChars, SystCons, SysCntxt), IntroSec(..), IntroSub(..), LFunc(..),
  Literature(Doc', Lit,Manual), RefSec(..), RefTab(..), StkhldrSec(..),
  StkhldrSub(Client, Cstmr), TConvention(..), TraceabilitySec(TraceabilityProg),
  TSIntro(..), TUIntro(..), PurposeDescription(..))
import Drasil.SRS.DocumentLanguage.Definitions (Field(..), Fields, InclUnits(IncludeUnits),
  Verbosity(..), ddefn)
import Drasil.SRS.DocumentLanguage.TraceabilityGraph (mkGraphInfo, traceyGraphGetRefs)
import Drasil.SRS.Sections.AuxiliaryConstants (tableOfConstants)
import Drasil.SRS.Sections.ReferenceMaterial (intro, emptySectSentPlu, emptySectSentSing)
import Drasil.SRS.Sections.Requirements (mkInputPropsTable,
  mkQRTuple, mkQRTupleRef, mkValsSourceTable, reqInputsRef, mkMaintainableNFR, mkPortableNFR, mkCorrectNFR,
  mkVerifiableNFR, mkUnderstandableNFR, mkReusableNFR, mkSecurityNFR, inReqWTab)
import Drasil.SRS.Sections.SpecificSystemDescription (auxSpecSent, termDefnF', inDataConstTbl, outDataConstTbl)
import Drasil.SRS.Sections.TableOfSymbols (tsymb, tsymb'')
import Drasil.SRS.Sections.TableOfUnits (unitTableRef, tunit, tunit',tunitNone)
import Drasil.SRS.Sections.TraceabilityMandGs (traceMatStandard, traceMatOtherReq)
import Drasil.SRS.SmithEtAlSRS
import Drasil.SRS.TraceTable (generateTraceMap)
