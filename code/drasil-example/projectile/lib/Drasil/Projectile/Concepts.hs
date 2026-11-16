module Drasil.Projectile.Concepts (
  launcher, projectile, target, projMotion, defs, rectVel, ideaDicts, launch,
  launAngleTerm, launAngleDef
) where

import Drasil.Database (mkUid)
import Control.Lens ((^.))
import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Development as D
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Math (angle)
import Data.Drasil.Concepts.Physics (motion, rectilinear, velocity)

ideaDicts :: [IdeaDict]
ideaDicts = [projMotion, launch, rectVel]

launch :: IdeaDict
launch = idea' (mkUid "launch") (nounPhraseSP "launch")

rectVel :: IdeaDict
rectVel = compoundNC rectilinear velocity

projMotion :: IdeaDict
projMotion = compoundNC projectile motion
---

defs :: [ConceptChunk]
defs = [launcher, projectile, target]

launcher, projectile, target :: ConceptChunk
launcher   = cncpt''' (mkUid "launcher")        (nounPhraseSP "launcher")
  (S "where the projectile is launched from and the device that does the launching")
projectile = cncpt''' (mkUid "projectile")      (nounPhraseSP "projectile")
  (S "the object to be launched at the target")
target     = cncpt''' (mkUid "target")          (nounPhraseSP "target")
  (S "where the projectile should be launched to")
launAngleTerm :: NP
launAngleTerm = compoundPhrase (launch ^. term) (angle ^. term)

launAngleDef :: Sentence
launAngleDef = foldlSent_ [D.toSent $ phraseNP (the angle), S "between the", phrase launcher
    `S.and_` S "a straight line" `S.fromThe` D.toSent (phraseNP (launcher `toThe` target))]
