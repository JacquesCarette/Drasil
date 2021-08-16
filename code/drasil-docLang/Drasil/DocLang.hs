-- | Re-export document language types and functions for easy use in other packages.
module Drasil.DocLang (
  -- * Document Language
  -- ** SRS
  -- *** Types

  -- DocDecl
  SRSDecl, DocSection(..), ReqrmntSec(..), ReqsSub(..),
  PDSub(..), ProblemDescription(..), SSDSec(..), SSDSub(..), SCSSub(..),
  SolChSpec(..),
  -- DocumentLanguage.Core
  AppndxSec(..), AuxConstntSec(..), DerivationDisplay(..), Emphasis(..),
  OffShelfSolnsSec(..), GSDSec(..), GSDSub(UsrChars, SystCons, SysCntxt),
  IntroSec(..), IntroSub(..), LFunc(..), Literature(Doc', Lit,Manual),
  RefSec(..), RefTab(..), StkhldrSec(..), StkhldrSub(Client, Cstmr),
  TConvention(..), TraceabilitySec(TraceabilityProg), TSIntro(..), TUIntro(..),
  -- *** Functions

  -- DocumentLanguage
  mkDoc, fillTraceSI, fillcdbSRS, findAllRefs,
  -- ** Notebook
  -- | For generating lesson plans.

  -- *** Types

  -- NBDecl
  NBDecl, NbSection(BibSec, IntrodSec, BodySec),
  -- DocumentLanguage.Notebook.Core
  IntrodSec(..), IntrodSub(..), BodySec(..), BodySub(..), SmmrySec(..), ApndxSec(..),
  -- *** Functions

  -- DocumentLanguage.Notebook.DocumentLanguage
  mkNb,
  -- * Subsection Functions
  -- ** Definitions and Models

  -- DocumentLanguage.Definitions
  Field(..), Fields, InclUnits(IncludeUnits), Verbosity(..), ddefn,
  -- DocumentLanguage.RefHelpers 
  ModelDB, ddRefDB, mdb,
  -- ** Traceability

  -- DocumentLanguage.TraceabilityGraph
  mkGraphInfo, traceyGraphGetRefs,
  -- Sections.TraceabilityMandGs
  traceMatStandard, traceMatOtherReq,
  -- Tracetable
  generateTraceMap,
  -- ** Auxiliary Constants

  -- Sections.AuxiliaryConstants
  tableOfConstants,
  -- ** Introduction

  -- Sections.Introduction
  purpDoc,
  -- ** Reference Material

  -- Sections.ReferenceMaterial
  intro,
  -- Sections.TableOfAbbAndAcronyms
  tableAbbAccRef,
  -- Sections.TableOfSymbols
  symbTableRef, tsymb, tsymb'',
  -- Sections.TableOfUnits
  unitTableRef, tunit, tunit',
  -- ** Requirements

  -- Sections.Requirements
  inReq, inTable, mkInputPropsTable, mkQRTuple, mkQRTupleRef, mkValsSourceTable, reqInputsRef,
  -- ** Specific System Description

  -- Sections.SpecificSystemDescription
  auxSpecSent, termDefnF', inDataConstTbl, outDataConstTbl,
  -- * Document Extraction Function

  -- ExtractDocDesc
  getDocDesc, egetDocDesc,
  -- * References

  -- References
  secRefs
) where 

import Drasil.DocDecl (SRSDecl, DocSection(..), ReqrmntSec(..), ReqsSub(..),
  PDSub(..), ProblemDescription(..), SSDSec(..), SSDSub(..), SCSSub(..),
  SolChSpec(..))
import Drasil.DocumentLanguage (mkDoc, fillTraceSI, fillcdbSRS, findAllRefs)
import Drasil.DocumentLanguage.Core (AppndxSec(..), AuxConstntSec(..),
  DerivationDisplay(..), Emphasis(..), OffShelfSolnsSec(..), GSDSec(..),
  GSDSub(UsrChars, SystCons, SysCntxt), IntroSec(..), IntroSub(..), LFunc(..),
  Literature(Doc', Lit,Manual), RefSec(..), RefTab(..), StkhldrSec(..),
  StkhldrSub(Client, Cstmr), TConvention(..), TraceabilitySec(TraceabilityProg),
  TSIntro(..), TUIntro(..))
import Drasil.DocumentLanguage.Notebook.Core (IntrodSec(..), IntrodSub(..), BodySec(..), 
  BodySub(..), ApndxSec(..), SmmrySec(..))
import Drasil.DocumentLanguage.Notebook.DocumentLanguage (mkNb)
import Drasil.DocumentLanguage.Notebook.NBDecl (NBDecl, NbSection(BibSec, IntrodSec, BodySec))
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
import Drasil.DocLang.References (secRefs)
-- Commented out modules aren't used - uncomment if this changes
