{-# LANGUAGE GADTs #-}
module Drasil.DocDecl where

import Drasil.DocumentLanguage.Core (DocDesc)
import qualified Drasil.DocumentLanguage.Core as DL (DocSection(..), RefSec(..),
  IntroSec(..), StkhldrSec(..), GSDSec(..), SSDSec(..), ReqrmntSec(..), ReqsSub(..),
  LCsSec(..), UCsSec(..), TraceabilitySec(..), AuxConstntSec(..), AppndxSec(..),
  OffShelfSolnsSec(..))

import Database.Drasil (ChunkDB, UMap, asOrderedList, conceptinsTable)
import Language.Drasil hiding (sec)

import Data.Drasil.Concepts.Documentation (funcReqDom, nonFuncReqDom, likeChgDom, unlikeChgDom)

import Control.Lens((^.), Getting)

type SRSDecl = [DocSection]

data DocSection = RefSec DL.RefSec
                | IntroSec DL.IntroSec
                | StkhldrSec DL.StkhldrSec
                | GSDSec DL.GSDSec
                | SSDSec DL.SSDSec
                | ReqrmntSec ReqrmntSec
                | LCsSec
                | UCsSec
                | TraceabilitySec DL.TraceabilitySec
                | AuxConstntSec DL.AuxConstntSec
                | Bibliography
                | AppndxSec DL.AppndxSec
                | OffShelfSolnsSec DL.OffShelfSolnsSec

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
  sec (SSDSec s) = DL.SSDSec s
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
  expandFromDB :: ([a] -> [a]) -> Getting (UMap a) ChunkDB (UMap a) -> [a]
  expandFromDB f = f . asOrderedList . (cdb ^.)
  fromConcInsDB :: Concept c => c -> [ConceptInstance]
  fromConcInsDB c = expandFromDB (filter (\x -> sDom (cdom x) == c ^. uid)) conceptinsTable
