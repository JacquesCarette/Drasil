-- | Re-export document language types and functions for easy use in other packages.
module Drasil.DocLang (
  -- * Document Language
  -- ** SRS
  -- | For generating Software Requirements Specifications.

  -- *** Types
  -- Drasil.DocDecl
  SRSDecl, DocSection(..), ProblemDescription(..), SolChSpec(..),
  TermsAndDefs(..), PhySysDesc(..), Goals(..),  
  Assumptions(..), TMs(..), GDs(..), DDs(..), IMs(..), Constraints(..), CorrSolnPpties(..),
  FReqsSub'(..), FReqsSub(..), NonFReqsSub(..),
  -- Drasil.DocumentLanguage.Core
  AppndxSec(..), AuxConstntSec(..), DerivationDisplay(..), Emphasis(..),
  OffShelfSolnsSec(..), GSDSec(..), UsrChars(..), SystCons(..), SysCntxt(..),
  IntroSec(..), IPurposeSub(..), IScopeSub(..), ICharSub(..), IOrgSub(..),
  SSDSec(..), ReqrmntSec(..), LFunc(..), Literature(Doc', Lit,Manual),
  RefSec(..), TUnits(..), TUnits'(..), TSymb(..), TSymb'(..), TAandA(..),
  StkhldrSec(..), ClientSub(..), CstmrSub(..),
  TConvention(..), TraceabilitySec(TraceabilityProg), TSIntro(..), TUIntro(..),
  -- *** Functions
  -- Drasil.DocumentLanguage
  mkDoc, fillTraceSI, fillcdbSRS, findAllRefs,
  -- ** Notebook
  -- | For generating Jupyter notebook lesson plans.

  -- *** Types
  -- Drasil.DocumentLanguage.Notebook.NBDecl
  NBDecl, NbSection(BibSec, IntrodSec, BodySec, 
  ReviewSub, MainIdeaSub, SupportSS1, SupportSS2, SupportSS3,
  MethsAnlsSub, ExampleSub, ApndxSec, SmmrySec),
  -- Drasil.DocumentLanguage.Notebook.Core
  IntrodSec(..), InPurposeSub(..), BodySec(..), 
  ReviewSub(..), MainIdeaSub(..), SupportSS1(..), SupportSS2(..), SupportSS3(..),
  MethsAnlsSub(..), ExampleSub(..), ApndxSec(..), SmmrySec(..),
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
  intro,
  -- Drasil.Sections.TableOfSymbols
  tsymb, tsymb'',
  -- Drasil.Sections.TableOfUnits
  unitTableRef, tunit, tunit',
  -- ** Requirements
  -- Drasil.Sections.Requirements
  inReq, inTable, mkInputPropsTable, mkQRTuple, mkQRTupleRef, mkValsSourceTable, reqInputsRef,
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

import Drasil.DocDecl (SRSDecl, DocSection(..), ProblemDescription(..), SolChSpec(..),
  TermsAndDefs(..), PhySysDesc(..), Goals(..),  
  Assumptions(..), TMs(..), GDs(..), DDs(..), IMs(..), Constraints(..), CorrSolnPpties(..),
  FReqsSub'(..), FReqsSub(..), NonFReqsSub(..),)
import Drasil.DocumentLanguage (mkDoc, fillTraceSI, fillcdbSRS, findAllRefs)
import Drasil.DocumentLanguage.Core (AppndxSec(..), AuxConstntSec(..),
  DerivationDisplay(..), Emphasis(..), OffShelfSolnsSec(..), GSDSec(..),
  UsrChars(..), SystCons(..), SysCntxt(..), IntroSec(..), 
  IPurposeSub(..), IScopeSub(..), ICharSub(..), IOrgSub(..),
  LFunc(..), Literature(Doc', Lit,Manual), 
  RefSec(..), TUnits(..), TUnits'(..), TSymb(..), TSymb'(..), TAandA(..),
  StkhldrSec(..), ClientSub(..), CstmrSub(..), TConvention(..), TraceabilitySec(TraceabilityProg),
  TSIntro(..), TUIntro(..), SSDSec(..), ReqrmntSec(..),)
import Drasil.DocumentLanguage.Notebook.Core (IntrodSec(..), InPurposeSub(..), BodySec(..), 
  ReviewSub(..), MainIdeaSub(..),  SupportSS1(..), SupportSS2(..), SupportSS3(..),
  MethsAnlsSub(..), ExampleSub(..), ApndxSec(..), SmmrySec(..))
import Drasil.DocumentLanguage.Notebook.DocumentLanguage (mkNb)
import Drasil.DocumentLanguage.Notebook.NBDecl (NBDecl, NbSection(BibSec, IntrodSec, BodySec, 
  ReviewSub, MainIdeaSub, SupportSS1, SupportSS2, SupportSS3, MethsAnlsSub, ExampleSub, ApndxSec, SmmrySec))
import Drasil.DocumentLanguage.Definitions (Field(..), Fields, InclUnits(IncludeUnits),
  Verbosity(..), ddefn)
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
--import Drasil.Sections.TableOfAbbAndAcronyms
import Drasil.Sections.TableOfSymbols (tsymb, tsymb'')
import Drasil.Sections.TableOfUnits (unitTableRef, tunit, tunit')
import Drasil.Sections.TraceabilityMandGs (traceMatStandard, traceMatOtherReq)
import Drasil.ExtractDocDesc (getDocDesc, egetDocDesc)
import Drasil.TraceTable (generateTraceMap)
import Drasil.DocLang.References (secRefs)
-- Commented out modules aren't used - uncomment if this changes
