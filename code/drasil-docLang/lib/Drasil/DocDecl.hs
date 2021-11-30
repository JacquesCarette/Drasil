{-# LANGUAGE GADTs #-}
-- | Document declaration types and functions for generating Software Requirement Specifications.
module Drasil.DocDecl where

import Drasil.DocumentLanguage.Core (DocDesc)
import Drasil.DocumentLanguage.Definitions (Fields)
import qualified Drasil.DocumentLanguage.Core as DL (DocSection(..), RefSec(..),
  IntroSec(..), StkhldrSec(..), GSDSec(..), SSDSec(..), 
  ProblemDescription(..), SolChSpec(..), TermsAndDefs(..), PhySysDesc(..), Goals(..), 
  Assumptions(..), TMs(..), GDs(..), DDs(..), IMs(..), Constraints(..), CorrSolnPpties(..), 
  ReqrmntSec(..), ReqsSub(..), LCsSec(..), UCsSec(..), TraceabilitySec(..), 
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
data DocSection = TableOfContents                       -- ^ Table of Contents
                | RefSec DL.RefSec                      -- ^ Reference.
                | IntroSec DL.IntroSec                  -- ^ Introduction.
                | StkhldrSec DL.StkhldrSec              -- ^ Stakeholders.
                | GSDSec DL.GSDSec                      -- ^ General System Description.
                | SSDSec DL.SSDSec                         -- ^ Specific System Description.
                | ProblemDescription ProblemDescription 
                | TermsAndDefs TermsAndDefs
                | PhySysDesc PhySysDesc
                | Goals Goals   
                | SolChSpec SolChSpec 
                | Assumptions Assumptions
                | TMs TMs
                | GDs GDs
                | DDs DDs
                | IMs IMs
                | Constraints Constraints
                | CorrSolnPpties CorrSolnPpties
                | ReqrmntSec ReqrmntSec                 -- ^ Requirements.
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
newtype ReqrmntSec = ReqsProg [ReqsSub]


-- | Requirements subsections.
data ReqsSub where
  -- | Functional requirements. 'LabelledContent' for tables (includes input values).
  FReqsSub    :: Sentence -> [LabelledContent] -> ReqsSub
  -- | Functional requirements. 'LabelledContent' for tables (no input values).
  FReqsSub'   :: [LabelledContent] -> ReqsSub
  -- | Non-Functional requirements.
  NonFReqsSub :: ReqsSub

-- * Functions

-- | Creates the document description (translates 'SRSDecl' into a more usable form for generating documents).
mkDocDesc :: SystemInformation -> SRSDecl -> DocDesc
mkDocDesc SI{_inputs = is, _sysinfodb = db} = map sec where
  sec :: DocSection -> DL.DocSection
  sec TableOfContents = DL.TableOfContents
  sec (RefSec r) = DL.RefSec r
  sec (IntroSec i) = DL.IntroSec i
  sec (StkhldrSec s) = DL.StkhldrSec s
  sec (GSDSec g) = DL.GSDSec g
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
  sec (ReqrmntSec (ReqsProg r)) = DL.ReqrmntSec $ DL.ReqsProg $ map reqSec r
  sec LCsSec = DL.LCsSec $ DL.LCsProg $ fromConcInsDB likeChgDom
  sec UCsSec = DL.UCsSec $ DL.UCsProg $ fromConcInsDB unlikeChgDom
  sec (TraceabilitySec t) = DL.TraceabilitySec t
  sec (AuxConstntSec a) = DL.AuxConstntSec a
  sec Bibliography = DL.Bibliography
  sec (AppndxSec a) = DL.AppndxSec a
  sec (OffShelfSolnsSec e) = DL.OffShelfSolnsSec e
  reqSec :: ReqsSub -> DL.ReqsSub
  reqSec (FReqsSub d t) = DL.FReqsSub (fullReqs is d $ fromConcInsDB funcReqDom) (fullTables is t)
  reqSec (FReqsSub' t) = DL.FReqsSub' (fromConcInsDB funcReqDom) t
  reqSec NonFReqsSub = DL.NonFReqsSub $ fromConcInsDB nonFuncReqDom
  expandFromDB :: ([a] -> [a]) -> Getting (UMap a) ChunkDB (UMap a) -> [a]
  expandFromDB f = f . asOrderedList . (db ^.)
  allInDB :: Getting (UMap a) ChunkDB (UMap a) -> [a]
  allInDB = expandFromDB id
  fromConcInsDB :: Concept c => c -> [ConceptInstance]
  fromConcInsDB c = expandFromDB (filter (\x -> sDom (cdom x) == c ^. uid)) conceptinsTable