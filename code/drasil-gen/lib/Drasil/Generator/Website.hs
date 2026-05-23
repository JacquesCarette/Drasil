{-# LANGUAGE QuasiQuotes #-}

module Drasil.Generator.Website
  ( -- * Drasil's website generator
    genWebsite,
  )
where

import Control.Lens ((^.))
import Text.PrettyPrint (Doc)

import Drasil.Build.Artifacts (FileLayout, file, ps)
import Drasil.System (DrasilWebsite, systemdb, webRefs)
import Language.Drasil (Document, Stage (Equational))
import Language.Drasil.Printers (Notation (Engineering), genHTML, makeCSS, piSys)
import Language.Drasil.Printing.Import (makeDocument)

-- | Generate Drasil's website (an HTML file with a CSS stylesheet).
genWebsite :: DrasilWebsite -> Document -> [FileLayout Doc]
genWebsite syst doc =
  [ file [ps|index.html|] $ genHTML "index" pd,
    file [ps|index.css|] $ makeCSS doc
  ]
  where
    printSetting = piSys (syst ^. systemdb) (syst ^. webRefs) Equational Engineering []
    pd = makeDocument printSetting doc
