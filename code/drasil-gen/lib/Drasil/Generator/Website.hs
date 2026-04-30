module Drasil.Generator.Website (
  exportWebsite
) where

import Control.Lens ((^.))

import Drasil.Build.Artifacts (createDirIfMissing, directory, file, writeArtifact)
import Language.Drasil (Document, Stage (Equational))
import Language.Drasil.Printers (Notation (Engineering), genHTML, makeCSS, piSys)
import Language.Drasil.Printing.Import (makeDocument)
import Drasil.System (DrasilWebsite, systemdb, webRefs)

import Drasil.Generator.Formats (Filename)

-- | Generate a "website" (an HTML file with a CSS stylesheet) softifact.
exportWebsite :: DrasilWebsite -> Document -> Filename -> IO ()
exportWebsite syst doc fileName = do
  let printSetting = piSys (syst ^. systemdb) (syst ^. webRefs) Equational Engineering []
      baseDir = "Website"
      pd = makeDocument printSetting doc
      website =
        directory
          "HTML"
          [ file (fileName ++ ".html") $ genHTML fileName pd,
            file (fileName ++ ".css") $ makeCSS doc
          ]

  createDirIfMissing True baseDir
  writeArtifact baseDir website
