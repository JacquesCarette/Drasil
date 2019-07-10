{-# LANGUAGE GADTs #-}
module Drasil.DocDecl where

import Drasil.DocumentLanguage.Core (DocDesc)
import Drasil.DocumentLanguage.Definitions (Fields)
import qualified Drasil.DocumentLanguage.Core as DL (DocSection(..), RefSec(..),
  IntroSec(..), StkhldrSec(..), GSDSec(..), SSDSec(..), SSDSub(..),
  ProblemDescription(..), PDSub(..), SolChSpec(..), SCSSub(..), ReqrmntSec(..),
  ReqsSub(..), LCsSec(..), UCsSec(..), TraceabilitySec(..), AuxConstntSec(..),
  AppndxSec(..), OffShelfSolnsSec(..), DerivationDisplay)

import Database.Drasil (ChunkDB, UMap, asOrderedList, conceptinsTable,
  dataDefnTable, gendefTable, insmodelTable, theoryModelTable)
import Language.Drasil hiding (sec)

import Data.Drasil.Concepts.Documentation (assumpDom, funcReqDom, goalStmtDom,
  nonFuncReqDom, likeChgDom, unlikeChgDom)

import Control.Lens((^.), Getting)

type SRSDecl = [DocSection]

data DocSection = RefSec DL.RefSec
                | IntroSec DL.IntroSec
                | StkhldrSec DL.StkhldrSec
                | GSDSec DL.GSDSec
                | SSDSec SSDSec
                | ReqrmntSec ReqrmntSec
                | LCsSec
                | UCsSec
                | TraceabilitySec DL.TraceabilitySec
                | AuxConstntSec DL.AuxConstntSec
                | Bibliography
                | AppndxSec DL.AppndxSec
                | OffShelfSolnsSec DL.OffShelfSolnsSec

-- | Specific System Description section . Contains a list of subsections.
newtype SSDSec = SSDProg [SSDSub]

-- | Specific system description subsections
data SSDSub where
  SSDProblem :: ProblemDescription -> SSDSub
  SSDSolChSpec :: SolChSpec -> SSDSub

-- | Problem Description section
data ProblemDescription where
  PDProg :: Sentence -> [Section] -> [PDSub] -> ProblemDescription

-- | Problem Description subsections
data PDSub where
  TermsAndDefs :: Concept c => Maybe Sentence -> [c] -> PDSub
  PhySysDesc :: Idea a => a -> [Sentence] -> LabelledContent -> [Contents] -> PDSub
  Goals :: [Sentence] -> PDSub

-- | Solution Characteristics Specification section
data SolChSpec where
  SCSProg :: [SCSSub] -> SolChSpec

-- | Solution Characteristics Specification subsections
data SCSSub where
  Assumptions    :: SCSSub
  TMs            :: [Sentence] -> Fields  -> SCSSub
  GDs            :: [Sentence] -> Fields  -> DL.DerivationDisplay -> SCSSub
  DDs            :: [Sentence] -> Fields  -> DL.DerivationDisplay -> SCSSub
  IMs            :: [Sentence] -> Fields  -> DL.DerivationDisplay -> SCSSub
  Constraints    :: Sentence -> Sentence -> Sentence -> [LabelledContent] -> SCSSub
  CorrSolnPpties :: [Contents] -> SCSSub

newtype ReqrmntSec = ReqsProg [ReqsSub]

data ReqsSub where
  FReqsSub    :: [LabelledContent] -> ReqsSub -- LabelledContent for tables
  NonFReqsSub :: ReqsSub

mkDocDesc :: ChunkDB -> SRSDecl -> DocDesc
mkDocDesc cdb = map sec where
  sec :: DocSection -> DL.DocSection
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
  sec Bibliography = DL.Bibliography
  sec (AppndxSec a) = DL.AppndxSec a
  sec (OffShelfSolnsSec e) = DL.OffShelfSolnsSec e
  reqSec :: ReqsSub -> DL.ReqsSub
  reqSec (FReqsSub t) = DL.FReqsSub (fromConcInsDB funcReqDom) t
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
  scsSub (Constraints ls s1 s2 lc) = DL.Constraints ls s1 s2 lc
  scsSub (CorrSolnPpties c) = DL.CorrSolnPpties c
  expandFromDB :: ([a] -> [a]) -> Getting (UMap a) ChunkDB (UMap a) -> [a]
  expandFromDB f = f . asOrderedList . (cdb ^.)
  allInDB :: Getting (UMap a) ChunkDB (UMap a) -> [a]
  allInDB = expandFromDB id
  fromConcInsDB :: Concept c => c -> [ConceptInstance]
  fromConcInsDB c = expandFromDB (filter (\x -> sDom (cdom x) == c ^. uid)) conceptinsTable
