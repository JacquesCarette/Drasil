{-# LANGUAGE QuasiQuotes #-}

module Drasil.Generator.LessonPlan (
  exportLessonPlan
) where

import Control.Lens ((^.))
import Data.Char (toLower)
import System.Directory (getCurrentDirectory, setCurrentDirectory, createDirectoryIfMissing)

import Drasil.Build.Artifacts (directory, file, localPath, ps, writeFiles)
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
      printSetting = piSys (plan ^. systemdb) (plan ^. lsnPlanRefs) Equational Engineering []
      pd = makeDocument printSetting nb
      exampleName = map toLower $ takeWhile (/= '_') lsnFileName
      artifact =
        directory
          [ps|Lesson|]
          [ file [ps|{lsnFileName}.ipynb|] $ genJupyterLessonPlan pd
          ]

  workingDir <- getCurrentDirectory
  createDirectoryIfMissing False exampleName
  setCurrentDirectory exampleName
  writeFiles localPath artifact
  setCurrentDirectory workingDir
