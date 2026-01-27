module Drasil.Generator.LessonPlan (
  exportLessonPlan
) where

import Prelude hiding (id)
import Control.Lens ((^.))
import System.IO (hClose, hPutStrLn, openFile, IOMode(WriteMode))
import Text.PrettyPrint.HughesPJ (render)

import Drasil.DocumentLanguage.Notebook (LsnDesc, mkNb)
import Language.Drasil (Stage(Equational))
import qualified Language.Drasil.Sentence.Combinators as S
import Language.Drasil.Printers (defaultConfiguration, piSys,
  genJupyterLessonPlan)
import Language.Drasil.Printing.Import (makeDocument)
import Drasil.System (System, refTable, systemdb)
import Utils.Drasil (createDirIfMissing)

-- | Generate an /interactive/ JupyterNotebook-based lesson plan.
exportLessonPlan :: System -> LsnDesc -> String -> IO ()
exportLessonPlan syst nbDecl lsnFileName = do
  let nb = mkNb nbDecl S.forT syst
      printSetting = piSys (syst ^. systemdb) (syst ^. refTable) Equational defaultConfiguration
      dir = "Lesson/"
      fn  = lsnFileName ++ ".ipynb"
      pd  = makeDocument printSetting nb

  createDirIfMissing True dir
  outh <- openFile (dir ++ "/" ++ fn) WriteMode
  hPutStrLn outh $ render $ genJupyterLessonPlan pd
  hClose outh
