{-# LANGUAGE QuasiQuotes #-}

module Drasil.Generator.Website
  ( -- * Drasil's website generator
    genWebsite,
  )
where

import Control.Lens ((^.))

import Drasil.FileHandling (FileLayout, file, ps)
import Drasil.System (DrasilWebsite, systemdb, webRefs, indexDoc)
import Language.Drasil (Stage (Equational))
import Language.Drasil.Printers (Notation (Engineering), genHTML, genericCSS, piSys)
import Language.Drasil.Printing.Import (makeDocument)

-- | Generate Drasil's website (an HTML file with a CSS stylesheet).
genWebsite :: DrasilWebsite -> [FileLayout]
genWebsite dw =
  [ file [ps|index.html|] $ genHTML "index" pd,
    file [ps|index.css|] genericCSS
  ]
  where
    printSetting = piSys (dw ^. systemdb) (dw ^. webRefs) Equational Engineering
    pd = makeDocument printSetting $ dw ^. indexDoc
