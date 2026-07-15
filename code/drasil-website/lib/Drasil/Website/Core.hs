{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Drasil.Website.Core
  ( DrasilWebsite,
    mkDrasilWebsite,
    indexDoc,
    defaultDrasilWebsiteGenOpts,
  )
where

import Control.Lens (makeLenses, (^.))
import Text.PrettyPrint (Doc)

import Drasil.FileHandling (file, ps)
import Language.Drasil (Stage (Equational))
import Language.Drasil.Document (Document)
import Language.Drasil.Printers (Notation (Engineering), genHTML, genericCSS,
  piSys, makeDocument)

import Drasil.System (HasSystemMeta (..), SystemMeta, ToFiles (..))

data DrasilWebsite = DW
  { _sm :: SystemMeta,
    _indexDoc :: Document
  }

makeLenses ''DrasilWebsite

instance HasSystemMeta DrasilWebsite where
  systemMeta = sm

mkDrasilWebsite :: SystemMeta -> Document -> DrasilWebsite
mkDrasilWebsite = DW

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
      printSetting = piSys (dw ^. systemdb) Equational Engineering
      pd = makeDocument printSetting $ dw ^. indexDoc

      -- 2. Transform the TDL into HTML.
      html = genHTML "index" pd
