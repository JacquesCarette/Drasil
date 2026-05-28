{-# LANGUAGE QuasiQuotes #-}

module Drasil.Generator.Website
  ( exportWebsite,
  )
where

import Control.Lens ((^.))
import Drasil.Build.Artifacts (OverwritePolicy (..), directory, file, localPath,
  ps, writeFiles)
import Drasil.Generator.Formats (Filename)
import Drasil.System (DrasilWebsite, systemdb, webRefs)
import Language.Drasil (Document, Stage (Equational))
import Language.Drasil.Printers (Notation (Engineering), genHTML, makeCSS, piSys)
import Language.Drasil.Printing.Import (makeDocument)

-- | Generate a "website" (an HTML file with a CSS stylesheet) softifact.
exportWebsite :: DrasilWebsite -> Document -> Filename -> IO ()
exportWebsite syst doc fileName = do
  let printSetting = piSys (syst ^. systemdb) (syst ^. webRefs) Equational Engineering
      pd = makeDocument printSetting doc
      website =
        directory
          [ps|Website|]
          [ directory
              [ps|HTML|]
              [ file [ps|{fileName}.html|] $ genHTML fileName pd,
                file [ps|{fileName}.css|] $ makeCSS doc
              ]
          ]

  writeFiles OverwriteAllowed localPath website
