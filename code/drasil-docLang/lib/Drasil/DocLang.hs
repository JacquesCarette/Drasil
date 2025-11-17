-- | Re-export document language types and functions for easy use in other packages.
module Drasil.DocLang (
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
  mkDoc, fillcdbSRS, findAllRefs,
  -- ** Notebook
  -- | For generating Jupyter notebook lesson plans.

  -- *** Types
  -- Drasil.DocumentLanguage.Notebook.LsnDecl
  LsnDecl, LsnChapter(Intro, LearnObj, Review, CaseProb, Example, Smmry, BibSec, Apndx),
  -- Drasil.DocumentLanguage.Notebook.Core
  Intro(..), LearnObj(..), Review(..), CaseProb(..), Example(..), Smmry(..), Apndx(..),
  -- *** Functions
  -- Drasil.DocumentLanguage.Notebook.DocumentLanguage
  mkNb,
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
  inReqWTab, mkInputPropsTable, mkQRTuple, mkQRTupleRef,
  mkValsSourceTable, reqInputsRef, mkMaintainableNFR, mkPortableNFR,
  mkCorrectNFR, mkVerifiableNFR, mkUnderstandableNFR, mkReusableNFR,
  mkSecurityNFR,
  -- ** Specific System Description
  -- Drasil.Sections.SpecificSystemDescription
  auxSpecSent, termDefnF', inDataConstTbl, outDataConstTbl,
  -- * Document Extraction Function
  -- Drasil.ExtractDocDesc
  getDocDesc, egetDocDesc,
  -- * References
  -- Drasil.SRS.References
  secRefs
) where

import Drasil.DocDecl (SRSDecl, DocSection(..), ReqrmntSec(..), ReqsSub(..),
  PDSub(..), ProblemDescription(..), SSDSec(..), SSDSub(..), SCSSub(..),
  SolChSpec(..))
import Drasil.DocumentLanguage (mkDoc, fillcdbSRS, findAllRefs)
import Drasil.DocumentLanguage.Core (AppndxSec(..), AuxConstntSec(..),
  DerivationDisplay(..), DocDesc, Emphasis(..), OffShelfSolnsSec(..), GSDSec(..),
  GSDSub(UsrChars, SystCons, SysCntxt), IntroSec(..), IntroSub(..), LFunc(..),
  Literature(Doc', Lit,Manual), RefSec(..), RefTab(..), StkhldrSec(..),
  StkhldrSub(Client, Cstmr), TConvention(..), TraceabilitySec(TraceabilityProg),
  TSIntro(..), TUIntro(..))
import Drasil.DocumentLanguage.Notebook.Core (Intro(..), LearnObj(..), Review(..),
  CaseProb(..), Example(..), Smmry(..), Apndx(..))
import Drasil.DocumentLanguage.Notebook.DocumentLanguage (mkNb)
import Drasil.DocumentLanguage.Notebook.LsnDecl (LsnDecl, LsnChapter(Intro, LearnObj, Review,
  CaseProb, Example, Smmry, BibSec, Apndx))
import Drasil.DocumentLanguage.Definitions (Field(..), Fields, InclUnits(IncludeUnits),
  Verbosity(..), ddefn)
--import Drasil.DocumentLanguage.TraceabilityMatrix
import Drasil.DocumentLanguage.TraceabilityGraph (mkGraphInfo, traceyGraphGetRefs)
import Drasil.Sections.AuxiliaryConstants (tableOfConstants)
--import Drasil.Sections.GeneralSystDesc
import Drasil.Sections.Introduction (purpDoc)
import Drasil.Sections.ReferenceMaterial (intro, emptySectSentPlu, emptySectSentSing)
import Drasil.Sections.Requirements (mkInputPropsTable,
  mkQRTuple, mkQRTupleRef, mkValsSourceTable, reqInputsRef, mkMaintainableNFR, mkPortableNFR, mkCorrectNFR,
  mkVerifiableNFR, mkUnderstandableNFR, mkReusableNFR, mkSecurityNFR, inReqWTab)
import Drasil.Sections.SpecificSystemDescription (auxSpecSent, termDefnF', inDataConstTbl, outDataConstTbl)
--import Drasil.Sections.Stakeholders
--import Drasil.Sections.TableOfAbbAndAcronyms
import Drasil.Sections.TableOfSymbols (tsymb, tsymb'')
import Drasil.Sections.TableOfUnits (unitTableRef, tunit, tunit',tunitNone)
import Drasil.Sections.TraceabilityMandGs (traceMatStandard, traceMatOtherReq)
import Drasil.ExtractDocDesc (getDocDesc, egetDocDesc)
import Drasil.TraceTable (generateTraceMap)
import Drasil.DocLang.References (secRefs)
-- Commented out modules aren't used - uncomment if this changes
