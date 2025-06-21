{-# LANGUAGE GADTs #-}
-- | Document declaration types and functions for generating Software Requirement Specifications.

-- Changes to DocSection and its subections should be reflected in the 'Creating Your Project 
-- in Drasil' tutorial found on the wiki:
-- https://github.com/JacquesCarette/Drasil/wiki/Creating-Your-Project-in-Drasil

module Drasil.DocDecl where

import Drasil.DocumentLanguage.Core (DocDesc)
import Drasil.DocumentLanguage.Definitions (Fields)
import qualified Drasil.DocumentLanguage.Core as DL (DocSection(..), RefSec(..),
  IntroSec(..), StkhldrSec(..), GSDSec(..), SSDSec(..), SSDSub(..),
  ProblemDescription(..), PDSub(..), SolChSpec(..), SCSSub(..), ReqrmntSec(..),
  ReqsSub(..), LCsSec(..), UCsSec(..), TraceabilitySec(..), AuxConstntSec(..),
  AppndxSec(..), OffShelfSolnsSec(..), DerivationDisplay)
import Drasil.Sections.Requirements (fullReqs, fullTables)

import Database.Drasil
import System.Drasil
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
                | SSDSec SSDSec                         -- ^ Specific System Description.
                | ReqrmntSec ReqrmntSec                 -- ^ Requirements.
                | LCsSec                                -- ^ Likely Changes.
                | UCsSec                                -- ^ Unlikely Changes.
                | TraceabilitySec DL.TraceabilitySec    -- ^ Traceability.
                | AuxConstntSec DL.AuxConstntSec        -- ^ Auxiliary Constants.
                | TAandA [IdeaDict]
                | Bibliography                          -- ^ Bibliography.
                | AppndxSec DL.AppndxSec                -- ^ Appendix.
                | OffShelfSolnsSec DL.OffShelfSolnsSec  -- ^ Off the Shelf Solutions.

-- | Specific System Description section (wraps 'SSDSub' subsections).
newtype SSDSec = SSDProg [SSDSub]

-- | Specific System Description subsections.
data SSDSub where
  -- | Problem description.
  SSDProblem :: ProblemDescription -> SSDSub
  -- | Solution characteristics.
  SSDSolChSpec :: SolChSpec -> SSDSub

-- | Problem Description section.
data ProblemDescription where
  PDProg :: Sentence -> [Section] -> [PDSub] -> ProblemDescription

-- | Problem Description subsections.
data PDSub where
  -- | Terms and Definitions.
  TermsAndDefs :: Concept c => Maybe Sentence -> [c] -> PDSub
  -- | Physical System Description.
  PhySysDesc :: Idea a => a -> [Sentence] -> LabelledContent -> [Contents] -> PDSub
  -- | Goals.
  Goals :: [Sentence] -> PDSub

-- | Solution Characteristics Specification section (wraps 'SCSSub' subsections).
data SolChSpec where
  SCSProg :: [SCSSub] -> SolChSpec

-- | Solution Characteristics Specification subsections.
data SCSSub where
  -- | Assumptions.
  Assumptions    :: SCSSub
  -- | Theory models.
  TMs            :: [Sentence] -> Fields  -> SCSSub
  -- | General definitions.
  GDs            :: [Sentence] -> Fields  -> DL.DerivationDisplay -> SCSSub
  -- | Data definitions.
  DDs            :: [Sentence] -> Fields  -> DL.DerivationDisplay -> SCSSub
  -- | Instance models.
  IMs            :: [Sentence] -> Fields  -> DL.DerivationDisplay -> SCSSub
  -- | Constraints.
  Constraints    :: (HasUncertainty c, Quantity c, Constrained c, HasReasVal c, MayHaveUnit c) => Sentence -> [c] -> SCSSub
  -- | Properties of a correct solution.
  CorrSolnPpties :: (Quantity c, Constrained c) => [c] -> [Contents] -> SCSSub

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
mkDocDesc :: System -> SRSDecl -> DocDesc
mkDocDesc SI{_inputs = is, _systemdb = db} = map sec where
  sec :: DocSection -> DL.DocSection
  sec TableOfContents = DL.TableOfContents
  sec (RefSec r) = DL.RefSec r
  sec (IntroSec i) = DL.IntroSec i
  sec (StkhldrSec s) = DL.StkhldrSec s
  sec (GSDSec g) = DL.GSDSec g
  sec (SSDSec (SSDProg s)) = DL.SSDSec $ DL.SSDProg $ map ssdSec s
  sec (ReqrmntSec (ReqsProg r)) = DL.ReqrmntSec $ DL.ReqsProg $ map reqSec r
  sec LCsSec = DL.LCsSec $ DL.LCsProg $ fromConcInsDB likeChgDom
  sec UCsSec = DL.UCsSec $ DL.UCsProg $ fromConcInsDB unlikeChgDom
  sec (TraceabilitySec t) = DL.TraceabilitySec t
  sec (AuxConstntSec a) = DL.AuxConstntSec a
  sec (TAandA ideas) = DL.TAandA ideas
  sec Bibliography = DL.Bibliography
  sec (AppndxSec a) = DL.AppndxSec a
  sec (OffShelfSolnsSec e) = DL.OffShelfSolnsSec e
  reqSec :: ReqsSub -> DL.ReqsSub
  reqSec (FReqsSub d t) = DL.FReqsSub (fullReqs is d $ fromConcInsDB funcReqDom) (fullTables is t)
  reqSec (FReqsSub' t) = DL.FReqsSub' (fromConcInsDB funcReqDom) t
  reqSec NonFReqsSub = DL.NonFReqsSub $ fromConcInsDB nonFuncReqDom
  ssdSec :: SSDSub -> DL.SSDSub
  ssdSec (SSDProblem (PDProg s ls p)) = DL.SSDProblem $ DL.PDProg s ls $ map pdSub p
  ssdSec (SSDSolChSpec (SCSProg scs)) = DL.SSDSolChSpec $ DL.SCSProg $ map scsSub scs
  pdSub :: PDSub -> DL.PDSub
  pdSub (TermsAndDefs s c) = DL.TermsAndDefs s c
  pdSub (PhySysDesc i s lc c) = DL.PhySysDesc i s lc c
  pdSub (Goals s) = DL.Goals s $ fromConcInsDB goalStmtDom
  scsSub :: SCSSub -> DL.SCSSub
  scsSub Assumptions = DL.Assumptions $ fromConcInsDB assumpDom
  scsSub (TMs s f) = DL.TMs s f $ allInDB theoryModelTable
  scsSub (GDs s f dd) = DL.GDs s f (allInDB gendefTable) dd
  scsSub (DDs s f dd) = DL.DDs s f (allInDB dataDefnTable) dd
  scsSub (IMs s f dd) = DL.IMs s f (allInDB insmodelTable) dd
  scsSub (Constraints s c) = DL.Constraints s c
  scsSub (CorrSolnPpties c cs) = DL.CorrSolnPpties c cs
  expandFromDB :: ([a] -> [a]) -> Getting (UMap a) ChunkDB (UMap a) -> [a]
  expandFromDB f = f . asOrderedList . (db ^.)
  allInDB :: Getting (UMap a) ChunkDB (UMap a) -> [a]
  allInDB = expandFromDB id
  fromConcInsDB :: Concept c => c -> [ConceptInstance]
  fromConcInsDB c = expandFromDB (filter (\x -> sDom (cdom x) == c ^. uid)) conceptinsTable
