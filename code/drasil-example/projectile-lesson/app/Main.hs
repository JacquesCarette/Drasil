module Main (main) where

import Drasil.Generator (generateRepo, drasilMakefileReqOpts)

import Drasil.LessonPlan (Options(..))
import Drasil.Projectile.Lesson.Body (si, nbDecl)
import qualified Language.Drasil.Sentence.Combinators as S

main :: IO ()
main = generateRepo si opts drasilMakefileReqOpts
  where
    opts = Options nbDecl S.forT "Projectile_Lesson"
