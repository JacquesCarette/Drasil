{-# LANGUAGE GADTs #-}
-- | Document declaration types and functions for generating Software Requirement Specifications.
module Drasil.DocDecl where

import Drasil.DocumentLanguage.Core (DocDesc)
import Drasil.DocumentLanguage.Definitions (Fields)
import qualified Drasil.DocumentLanguage.Core as DL (DocSection(..), TableOfContents(..),
  RefSec(..), TUnits(..), TUnits'(..), TSymb(..), TSymb'(..), TAandA(..),
  IntroSec(..), IPurposeSub(..), IScopeSub(..), ICharSub(..), IOrgSub(..),
  StkhldrSec(..), ClientSub(..), CstmrSub(..), GSDSec(..), SysCntxt(..), UsrChars(..), SystCons(..),
  SSDSec(..), LCsSec(..), UCsSec(..), TraceabilitySec(..), 
  ProblemDescription(..), SolChSpec(..), TermsAndDefs(..), PhySysDesc(..), Goals(..), 
  Assumptions(..), TMs(..), GDs(..), DDs(..), IMs(..), Constraints(..), CorrSolnPpties(..), 
  ReqrmntSec(..), FReqsSub'(..), FReqsSub(..), NonFReqsSub(..),  
  AuxConstntSec(..), AppndxSec(..), OffShelfSolnsSec(..), DerivationDisplay)
import Drasil.Sections.Requirements (fullReqs, fullTables)

import Database.Drasil (ChunkDB, SystemInformation(SI), UMap, asOrderedList,
  _inputs, _sysinfodb, conceptinsTable, dataDefnTable, gendefTable,
  insmodelTable, theoryModelTable)
import Language.Drasil hiding (sec)

import Data.Drasil.Concepts.Documentation (assumpDom, funcReqDom, goalStmtDom,
  nonFuncReqDom, likeChgDom, unlikeChgDom)

import Control.Lens((^.), Getting)

-- * Types

-- | A Software Requirements Specification Declaration is made up of all necessary sections ('DocSection's).
type SRSDecl = [DocSection]

-- | Contains all the different sections needed for a full SRS ('SRSDecl').
data DocSection = TableOfContents DL.TableOfContents    -- ^ Table of Contents
                | RefSec DL.RefSec                      -- ^ Reference.
                | TUnits DL.TUnits
                | TUnits' DL.TUnits'
                | TSymb DL.TSymb
                | TSymb' DL.TSymb'
                | TAandA DL.TAandA
                | IntroSec DL.IntroSec                  -- ^ Introduction.
                | IPurposeSub DL.IPurposeSub
                | IScopeSub DL.IScopeSub
                | ICharSub DL.ICharSub
                | IOrgSub DL.IOrgSub
                | StkhldrSec DL.StkhldrSec              -- ^ Stakeholders.
                | ClientSub DL.ClientSub 
                | CstmrSub DL.CstmrSub
                | GSDSec DL.GSDSec                      -- ^ General System Description.
                | SysCntxt DL.SysCntxt
                | UsrChars DL.UsrChars
                | SystCons DL.SystCons
                | SSDSec DL.SSDSec                      -- ^ Specific System Description.
                | ProblemDescription ProblemDescription -- ^ Problem Description
                | TermsAndDefs TermsAndDefs             -- ^ Terminology and Definitions
                | PhySysDesc PhySysDesc                 -- ^ Physical System Description
                | Goals Goals                           -- ^ Goal Statements
                | SolChSpec SolChSpec                   -- ^ Solution Characteristics Specification
                | Assumptions Assumptions               -- ^ Assumptions
                | TMs TMs                               -- ^ Theoretical Models
                | GDs GDs                               -- ^ General Definitions
                | DDs DDs                               -- ^ Data Definitions
                | IMs IMs                               -- ^ Instance Models
                | Constraints Constraints               -- ^ Data Constraints
                | CorrSolnPpties CorrSolnPpties         -- ^ Properties of a Correct Solution
                | ReqrmntSec DL.ReqrmntSec              -- ^ Requirements.
                | FReqsSub FReqsSub                     -- ^ Functional Requirements
                | FReqsSub' FReqsSub'                   -- ^ Functional Requirements
                | NonFReqsSub NonFReqsSub               -- ^ Non-Functional Requirements
                | LCsSec                                -- ^ Likely Changes.
                | UCsSec                                -- ^ Unlikely Changes.
                | TraceabilitySec DL.TraceabilitySec    -- ^ Traceability.
                | AuxConstntSec DL.AuxConstntSec        -- ^ Auxiliary Constants.
                | Bibliography                          -- ^ Bibliography.
                | AppndxSec DL.AppndxSec                -- ^ Appendix.
                | OffShelfSolnsSec DL.OffShelfSolnsSec  -- ^ Off the Shelf Solutions.

-- | Specific System Description section (wraps 'SSDSub' subsections).
--newtype SSDSec = SSDProg Sentence

-- | Problem Description section.
newtype ProblemDescription = PDProg Sentence

-- | Problem Description subsections.
-- | Terms and Definitions.
data TermsAndDefs where 
  TDProg :: Concept c => Maybe Sentence -> [c] -> TermsAndDefs
-- | Physical System Description.
data PhySysDesc where
  PSDProg :: Idea a => a -> [Sentence] -> LabelledContent -> [Contents] -> PhySysDesc
-- | Goals.
newtype Goals = GProg [Sentence]

-- | Solution Characteristics Specification section (wraps 'SCSSub' subsections).
newtype SolChSpec = SCSProg Sentence

-- | Solution Characteristics Specification subsections.
-- | Assumptions.
data Assumptions where AssumpProg :: Sentence -> Assumptions
-- | Theory models.
data TMs = TMProg [Sentence] Fields 
-- | General definitions.
data GDs = GDProg [Sentence] Fields DL.DerivationDisplay
-- | Data definitions.
data DDs = DDProg [Sentence] Fields DL.DerivationDisplay
-- | Instance models.
data IMs = IMProg [Sentence] Fields DL.DerivationDisplay
-- | Constraints.
data Constraints where
  ConstProg :: (HasUncertainty c, Quantity c, Constrained c, HasReasVal c, MayHaveUnit c) => Sentence -> [c] -> Constraints 
-- | Properties of a correct solution.
data CorrSolnPpties where
  CorrSolProg :: (Quantity c, Constrained c) => [c] -> [Contents] -> CorrSolnPpties


-- | Requirements section (wraps 'ReqsSub' subsections).
-- newtype ReqrmntSec = ReqsProg Sentence


-- | Requirements subsections.
-- | Functional requirements. 'LabelledContent' for tables (includes input values).
data FReqsSub  = FReqsProg Sentence [LabelledContent]
-- | Functional requirements. 'LabelledContent' for tables (no input values).
newtype FReqsSub' = FReqsProg' [LabelledContent]
-- | Non-Functional requirements.
data NonFReqsSub where NonFReqsProg :: NonFReqsSub


-- * Functions

-- | Creates the document description (translates 'SRSDecl' into a more usable form for generating documents).
mkDocDesc :: SystemInformation -> SRSDecl -> DocDesc
mkDocDesc SI{_inputs = is, _sysinfodb = db} = map sec where
  sec :: DocSection -> DL.DocSection
  sec (TableOfContents tc) = DL.TableOfContents tc
  sec (TUnits u) = DL.TUnits u
  sec (TUnits' u) = DL.TUnits' u
  sec (TSymb s) = DL.TSymb s
  sec (TSymb' s) = DL.TSymb' s
  sec (TAandA a) = DL.TAandA a
  sec (RefSec r) = DL.RefSec r
  sec (IntroSec i) = DL.IntroSec i
  sec (IPurposeSub p) = DL.IPurposeSub p
  sec (IScopeSub s) = DL.IScopeSub s
  sec (ICharSub c) = DL.ICharSub c
  sec (IOrgSub o) = DL.IOrgSub o
  sec (StkhldrSec s) = DL.StkhldrSec s
  sec (ClientSub c) = DL.ClientSub c
  sec (CstmrSub c) = DL.CstmrSub c
  sec (GSDSec g) = DL.GSDSec g
  sec (SysCntxt c) = DL.SysCntxt c
  sec (UsrChars c) = DL.UsrChars c
  sec (SystCons c) = DL.SystCons c
  sec (SSDSec s) = DL.SSDSec s
  sec (ProblemDescription (PDProg s)) = DL.ProblemDescription (DL.PDProg s)
  sec (TermsAndDefs (TDProg s c)) = DL.TermsAndDefs (DL.TDProg s c)
  sec (PhySysDesc (PSDProg i s lc c)) = DL.PhySysDesc (DL.PSDProg i s lc c)
  sec (Goals (GProg s)) = DL.Goals $ DL.GProg s $ fromConcInsDB goalStmtDom
  sec (SolChSpec (SCSProg s)) = DL.SolChSpec (DL.SCSProg s)
  sec (Assumptions (AssumpProg _)) = DL.Assumptions $ DL.AssumpProg $ fromConcInsDB assumpDom
  sec (TMs (TMProg s f)) = DL.TMs $ DL.TMProg s f $ allInDB theoryModelTable
  sec (GDs (GDProg s f dd)) = DL.GDs $ DL.GDProg s f (allInDB gendefTable) dd
  sec (DDs (DDProg s f dd)) = DL.DDs $ DL.DDProg s f (allInDB dataDefnTable) dd
  sec (IMs (IMProg s f dd)) = DL.IMs $ DL.IMProg s f (allInDB insmodelTable) dd
  sec (Constraints (ConstProg s c)) = DL.Constraints (DL.ConstProg s c)
  sec (CorrSolnPpties (CorrSolProg c cs)) = DL.CorrSolnPpties (DL.CorrSolProg c cs)
  sec (ReqrmntSec r) = DL.ReqrmntSec r
  sec (FReqsSub (FReqsProg d t)) = DL.FReqsSub $ DL.FReqsProg (fullReqs is d $ fromConcInsDB funcReqDom) (fullTables is t)
  sec (FReqsSub' (FReqsProg' t)) = DL.FReqsSub' $ DL.FReqsProg' (fromConcInsDB funcReqDom) t
  sec (NonFReqsSub NonFReqsProg) = DL.NonFReqsSub $ DL.NonFReqsProg $ fromConcInsDB nonFuncReqDom
  sec LCsSec = DL.LCsSec $ DL.LCsProg $ fromConcInsDB likeChgDom
  sec UCsSec = DL.UCsSec $ DL.UCsProg $ fromConcInsDB unlikeChgDom
  sec (TraceabilitySec t) = DL.TraceabilitySec t
  sec (AuxConstntSec a) = DL.AuxConstntSec a
  sec Bibliography = DL.Bibliography
  sec (AppndxSec a) = DL.AppndxSec a
  sec (OffShelfSolnsSec e) = DL.OffShelfSolnsSec e
  expandFromDB :: ([a] -> [a]) -> Getting (UMap a) ChunkDB (UMap a) -> [a]
  expandFromDB f = f . asOrderedList . (db ^.)
  allInDB :: Getting (UMap a) ChunkDB (UMap a) -> [a]
  allInDB = expandFromDB id
  fromConcInsDB :: Concept c => c -> [ConceptInstance]
  fromConcInsDB c = expandFromDB (filter (\x -> sDom (cdom x) == c ^. uid)) conceptinsTable