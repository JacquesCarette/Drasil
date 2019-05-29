module Drasil.Projectile.IMods (iMods) where

import Language.Drasil
import Theory.Drasil (InstanceModel, imNoDerivNoRefs)

import Drasil.Projectile.Unitals (isHit, isShort, launDist, offset, targDist)

iMods :: [InstanceModel]
iMods = [shortIM, offsetIM, hitIM]

---
shortIM :: InstanceModel
shortIM = imNoDerivNoRefs shortRC [qw targDist, qw launDist]
  [sy targDist $> 0, sy launDist $> 0] (qw isShort) []
  "shortIM" [shortDesc]

shortRC :: RelationConcept
shortRC = makeRC "shortRC" (nounPhraseSP "isShort") 
  shortDesc $ sy isShort $= sy targDist $> sy launDist
  
shortDesc :: Sentence
shortDesc = EmptyS

---
offsetIM :: InstanceModel
offsetIM = imNoDerivNoRefs offsetRC [qw targDist, qw launDist]
  [sy targDist $> 0, sy launDist $> 0] (qw offset) []
  "offsetIM" [offsetDesc]

offsetRC :: RelationConcept
offsetRC = makeRC "offsetRC" (nounPhraseSP "offset") 
  offsetDesc $ sy offset $= UnaryOp Abs (sy targDist - sy launDist)
  
offsetDesc :: Sentence
offsetDesc = EmptyS

---
hitIM :: InstanceModel
hitIM = imNoDerivNoRefs hitRC [qw offset, qw targDist]
  [sy offset $> 0, sy targDist $> 0] (qw isHit) []
  "hitIM" [hitDesc]

hitRC :: RelationConcept
hitRC = makeRC "hitRC" (nounPhraseSP "isHit") 
  hitDesc $ sy isHit $= sy offset $< (0.02 * sy targDist)
  
hitDesc :: Sentence
hitDesc = EmptyS

