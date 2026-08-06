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
import qualified Data.Map.Strict as M
import Drasil.Data.Formats.HTML (HTMLRenderOptions (..))
import Prettyprinter (Doc)

import Drasil.FileHandling (file, ps)
import Drasil.System (HasSystemMeta (..), SystemMeta, ToFiles (..))
import Language.Drasil (Stage (Equational))
import Language.Drasil.Document (Document)
import Language.Drasil.Printers (HTMLGenOptions (..), Notation (Engineering),
  genHTML, renderHTML, genericCSS, piSys, makeDocument)

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
    css :: Doc ()
  }

-- | Default options for the 'DrasilWebsite' generator.
defaultDrasilWebsiteGenOpts :: DrasilWebsiteGenOptions
defaultDrasilWebsiteGenOpts = DWGO genericCSS

instance ToFiles DrasilWebsite DrasilWebsiteGenOptions where
  toFiles dw DWGO {..} =
    [ file [ps|index.html|] renderedHTML,
      file [ps|index.css|] css
    ]
    where
      -- Steps:

      -- 1. Transform the Semantic-Document-Language-encoded website to the
      -- Typesetting Document Language (TDL).
      printSetting = piSys (dw ^. systemdb) Equational Engineering
      pd = makeDocument printSetting $ dw ^. indexDoc

      -- 2. Transform the TDL into HTML.
      html = genHTML
        (HTMLGO "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml-full.js")
        "index" pd
      renderedHTML = renderHTML (HTMLRO M.empty 2) html
