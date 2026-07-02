{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Drasil.System.DrasilWebsite
  ( DrasilWebsite,
    mkDrasilWebsite,
    indexDoc,
    webRefs,
    defaultDrasilWebsiteGenOpts,
  )
where

import Control.Lens (makeLenses, (^.))
import qualified Data.Map.Strict as M
import Text.PrettyPrint (Doc)

import Drasil.Database (UID, uid)
import Drasil.FileHandling (file, ps)
import Language.Drasil (Stage (Equational))
import Language.Drasil.Document (Document, Reference)
import Language.Drasil.Printers (Notation (Engineering), genHTML, genericCSS, piSys)
import Language.Drasil.Printing.Import (makeDocument)

import Drasil.System.Core (HasSystemMeta (..), SystemMeta)
import Drasil.System.Transformations (ToFiles (..))

data DrasilWebsite = DW
  { _sm :: SystemMeta,
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

-- | HTML generation options for the 'DrasilWebsite'.
newtype DrasilWebsiteGenOptions = DWGO
  { -- | What CSS should be loaded?
    css :: Doc
  }

-- | Default options for the 'DrasilWebsite' generator.
defaultDrasilWebsiteGenOpts :: DrasilWebsiteGenOptions
defaultDrasilWebsiteGenOpts = DWGO genericCSS

instance ToFiles DrasilWebsite DrasilWebsiteGenOptions where
  toFiles dw DWGO {..} =
    [ file [ps|index.html|] html,
      file [ps|index.css|] css
    ]
    where
      -- Steps:

      -- 1. Transform the Semantic-Document-Language-encoded website to the
      -- Typesetting Document Language (TDL).
      printSetting = piSys (dw ^. systemdb) (dw ^. webRefs) Equational Engineering
      pd = makeDocument printSetting $ dw ^. indexDoc

      -- 2. Transform the TDL into HTML.
      html = genHTML "index" pd
