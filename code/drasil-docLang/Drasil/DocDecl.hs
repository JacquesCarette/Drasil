module Drasil.DocDecl where

import Drasil.DocumentLanguage.Core (DocDesc)
import qualified Drasil.DocumentLanguage.Core as DL (DocSection(..), RefSec(..), IntroSec(..),
  StkhldrSec(..), GSDSec(..), SSDSec(..), ReqrmntSec(..), LCsSec(..), UCsSec(..),
  TraceabilitySec(..), AuxConstntSec(..), AppndxSec(..), OffShelfSolnsSec(..))

import Database.Drasil (ChunkDB, UMap, asOrderedList, conceptinsTable)
import Language.Drasil hiding (sec)

import Data.Drasil.Concepts.Documentation (likeChgDom)

import Control.Lens((^.), Getting)

type SRSDecl = [DocSection]

data DocSection = RefSec DL.RefSec
                | IntroSec DL.IntroSec
                | StkhldrSec DL.StkhldrSec
                | GSDSec DL.GSDSec
                | SSDSec DL.SSDSec
                | ReqrmntSec DL.ReqrmntSec
                | LCsSec
                | UCsSec DL.UCsSec
                | TraceabilitySec DL.TraceabilitySec
                | AuxConstntSec DL.AuxConstntSec
                | Bibliography
                | AppndxSec DL.AppndxSec
                | OffShelfSolnsSec DL.OffShelfSolnsSec

mkDocDesc :: ChunkDB -> SRSDecl -> DocDesc
mkDocDesc cdb = map sec where
  sec :: DocSection -> DL.DocSection
  sec (RefSec r) = DL.RefSec r
  sec (IntroSec i) = DL.IntroSec i
  sec (StkhldrSec s) = DL.StkhldrSec s
  sec (GSDSec g) = DL.GSDSec g
  sec (SSDSec s) = DL.SSDSec s
  sec (ReqrmntSec r) = DL.ReqrmntSec r
  sec LCsSec = DL.LCsSec $ DL.LCsProg $ fromConcInsDB likeChgDom
  sec (UCsSec u) = DL.UCsSec u
  sec (TraceabilitySec t) = DL.TraceabilitySec t
  sec (AuxConstntSec a) = DL.AuxConstntSec a
  sec Bibliography = DL.Bibliography
  sec (AppndxSec a) = DL.AppndxSec a
  sec (OffShelfSolnsSec e) = DL.OffShelfSolnsSec e
  expandFromDB :: ([a] -> [a]) -> Getting (UMap a) ChunkDB (UMap a) -> [a]
  expandFromDB f = f . asOrderedList . (cdb ^.)
  fromConcInsDB :: Concept c => c -> [ConceptInstance]
  fromConcInsDB c = expandFromDB (filter (\x -> sDom (cdom x) == c ^. uid)) conceptinsTable
