module Drasil.System.DrasilWebsite (
  DrasilWebsite,
  mkDrasilWebsite,
  indexDoc,
  webRefs,
) where

import Control.Lens (makeLenses, (^.))
import qualified Data.Map.Strict as M

import Drasil.Database (UID, uid)
import Language.Drasil.Document (Document, Reference)

import Drasil.System.Core (HasSystemMeta(..), SystemMeta)

data DrasilWebsite = DW {
  _sm :: SystemMeta,
  _indexDoc :: Document,
  _webRefs :: M.Map UID Reference
}

makeLenses ''DrasilWebsite

instance HasSystemMeta DrasilWebsite where
  systemMeta = sm

mkDrasilWebsite :: SystemMeta -> Document -> [Reference] -> DrasilWebsite
mkDrasilWebsite m doc rs = DW m doc refs
  where
    refs = M.fromList $ map (\r -> (r ^. uid, r)) rs
