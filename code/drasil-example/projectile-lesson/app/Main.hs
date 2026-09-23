module Main (main) where

import Drasil.Generator (concretizeAndWrite, drasilMakefileReqOpts)

import Drasil.LessonPlan (JupyterGenOptions(..))
import Drasil.Projectile.Lesson.Body (si, nbDecl)
import qualified Language.Drasil.Sentence.Combinators as S

main :: IO ()
main = concretizeAndWrite si opts drasilMakefileReqOpts
  where
    opts = JupyterGenOptions nbDecl S.forT' "Projectile_Lesson"
