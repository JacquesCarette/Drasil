module Drasil.Projectile.Assumptions (assumptions) where

import Language.Drasil

import Data.Drasil.Concepts.Documentation as Doc (assumpDom)

assumptions :: [ConceptInstance]
assumptions = [twoDMotion]

twoDMotion :: ConceptInstance
twoDMotion = cic "twoDMotion" twoDMotionDesc "twoDMotion" assumpDom

twoDMotionDesc :: Sentence
twoDMotionDesc = EmptyS