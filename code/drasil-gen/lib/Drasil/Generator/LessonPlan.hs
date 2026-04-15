module Drasil.Generator.LessonPlan (
  exportLessonPlan
) where

import Prelude hiding (id)
import Control.Lens ((^.))
import System.IO (hClose, hPutStrLn, openFile, IOMode(WriteMode))
import Text.PrettyPrint.HughesPJ (render)

import Drasil.Build.Artifacts (createDirIfMissing)
import Drasil.DocumentLanguage.Notebook (LsnDesc, mkNb)
import Language.Drasil (Stage(Equational))
import qualified Language.Drasil.Sentence.Combinators as S
import Language.Drasil.Printers (Notation(Engineering), piSys,
  genJupyterLessonPlan)
import Language.Drasil.Printing.Import (makeDocument)
import Drasil.System (LessonPlan, lsnPlanRefs, systemdb, LessonPlan)

-- | Generate an /interactive/ JupyterNotebook-based lesson plan.
exportLessonPlan :: LessonPlan -> LsnDesc -> String -> IO ()
exportLessonPlan plan nbDecl lsnFileName = do
  let nb = mkNb plan nbDecl S.forT
      printSetting = piSys (plan ^. systemdb) (plan ^. lsnPlanRefs) Equational Engineering []
      dir = "Lesson/"
      fn  = lsnFileName ++ ".ipynb"
      pd  = makeDocument printSetting nb

  createDirIfMissing True dir
  outh <- openFile (dir ++ "/" ++ fn) WriteMode
  hPutStrLn outh $ render $ genJupyterLessonPlan pd
  hClose outh
