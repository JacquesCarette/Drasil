module Drasil.System.DrasilWebsite (
  DrasilWebsite,
  mkDrasilWebsite,
  webRefs,
) where

import Control.Lens (makeLenses, (^.))
import qualified Data.Map.Strict as M

import Drasil.Database (UID, uid)
import Language.Drasil.Docs (Reference)

import Drasil.System.Core (HasSystemMeta(..), SystemMeta)

data DrasilWebsite = DW {
  _sm :: SystemMeta,
  _webRefs :: M.Map UID Reference
}

makeLenses ''DrasilWebsite

instance HasSystemMeta DrasilWebsite where
  systemMeta = sm

mkDrasilWebsite :: SystemMeta -> [Reference] -> DrasilWebsite
mkDrasilWebsite m rs = DW m refs
  where
    refs = M.fromList $ map (\r -> (r ^. uid, r)) rs
