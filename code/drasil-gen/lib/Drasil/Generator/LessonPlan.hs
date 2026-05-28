{-# LANGUAGE QuasiQuotes #-}

module Drasil.Generator.LessonPlan (
  exportLessonPlan
) where

import Control.Lens ((^.))

import Drasil.Build.Artifacts (OverwritePolicy (..), directory, file, localPath,
  ps, writeFiles)
import Drasil.DocumentLanguage.Notebook (LsnDesc, mkNb)
import Language.Drasil (Stage (Equational))
import Language.Drasil.Printers (Notation (Engineering), genJupyterLessonPlan, piSys)
import Language.Drasil.Printing.Import (makeDocument)
import qualified Language.Drasil.Sentence.Combinators as S
import Drasil.System (LessonPlan, lsnPlanRefs, systemdb)

-- | Generate an /interactive/ JupyterNotebook-based lesson plan.
exportLessonPlan :: LessonPlan -> LsnDesc -> String -> IO ()
exportLessonPlan plan nbDecl lsnFileName = do
  let nb = mkNb plan nbDecl S.forT
      printSetting = piSys (plan ^. systemdb) (plan ^. lsnPlanRefs) Equational Engineering
      pd = makeDocument printSetting nb
      artifact =
        directory
          [ps|Lesson|]
          [ file [ps|{lsnFileName}.ipynb|] $ genJupyterLessonPlan pd
          ]

  writeFiles OverwriteAllowed localPath artifact
