{-# LANGUAGE QuasiQuotes #-}

module Drasil.Generator.LessonPlan
  ( genJupyterLessonPlan,
  )
where

import Control.Lens ((^.))

import Drasil.FileHandling (FileLayout, file, ps)
import Drasil.LessonPlan (LsnDesc, render)
import Language.Drasil (Stage (Equational))
import Language.Drasil.Printers (Notation (Engineering), piSys)
import qualified Language.Drasil.Printers as P (genJupyterLessonPlan)
import Language.Drasil.Printing.Import (makeDocument)
import qualified Language.Drasil.Sentence.Combinators as S
import Drasil.System (LessonPlan, lsnPlanRefs, systemdb)

-- | Generate a Lesson Plan (an interactive JupyterNotebook).
genJupyterLessonPlan :: LessonPlan -> LsnDesc -> String -> FileLayout
genJupyterLessonPlan plan nbDecl lsnFileName =
  file [ps|{lsnFileName}.ipynb|] $ P.genJupyterLessonPlan pd
  where
    nb = render plan nbDecl S.forT
    printSetting = piSys (plan ^. systemdb) (plan ^. lsnPlanRefs) Equational Engineering
    pd = makeDocument printSetting nb
