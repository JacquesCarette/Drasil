module Drasil.Generator.Website (
  exportWebsite
) where

import Prelude hiding (id)
import Control.Lens ((^.))
import System.IO (hClose, hPutStrLn, openFile, IOMode(WriteMode))
import Text.PrettyPrint.HughesPJ (render)

import Language.Drasil (Stage(Equational), Document)
import Language.Drasil.Printers (makeCSS, genHTML, defaultConfiguration, piSys)
import Language.Drasil.Printing.Import (makeDocument)
import Drasil.System (System, refTable, systemdb)
import Utils.Drasil (createDirIfMissing)

import Drasil.Generator.Formats (Filename)

-- | Generate a "website" (HTML file) softifact.
exportWebsite :: System -> Document -> Filename -> IO ()
exportWebsite syst doc fileName = do
  let printSetting = piSys (syst ^. systemdb) (syst ^. refTable) Equational defaultConfiguration
      dir = "Website/HTML"
      pd = makeDocument printSetting doc

  createDirIfMissing True dir

  outh <- openFile (dir ++ "/" ++ fileName ++ ".html") WriteMode
  hPutStrLn outh $ render $ genHTML fileName pd
  hClose outh

  outh2 <- openFile (dir ++ "/" ++ fileName ++ ".css") WriteMode
  hPutStrLn outh2 $ render $ makeCSS doc
  hClose outh2
