{-# OPTIONS -Wall #-}
module Reference where

import LayoutObjs
import Spec
import Data.Char
import Helpers

-- Creating References --
makeRef :: LayoutObj -> Spec
makeRef r = Ref (rType r) (getRefName r)

rType :: LayoutObj -> RefType
rType (Table _ _ _ _) = Tab
rType (Figure _ _)    = Fig
rType (Section d _ _) = Sec d
rType _ = error "Attempting to reference unimplemented reference type"

getRefName :: LayoutObj -> Spec
getRefName (Table h d l b)  = S "Table:" :+: simplify l
getRefName (Section d t _)  = writeSec d :+: simplify t
getRefName (Figure l _)     = S "Figure:" :+: simplify l
getRefName (Paragraph c)    = error "Can't reference paragraphs" --yet
getRefName (EqnBlock c)     = error "EqnBlock ref unimplemented"
getRefName (CodeBlock c)    = error "Codeblock ref unimplemented"
getRefName (Definition d)   = error "Definition ref unimplemented"
getRefName (BulletList b)   = error "BulletList ref unimplemented"
getRefName (NumberedList i) = error "NumberedList ref unimplemented"
getRefName (SimpleList p)   = error "SimpleList ref unimplemented"

simplify :: Spec -> Spec
simplify (s1 :-: s2) = simplify s1 :-: simplify s2
simplify (s1 :^: s2) = simplify s1 :^: simplify s2
simplify (s1 :+: s2) = simplify s1 :+: simplify s2
simplify (s1 :/: s2) = simplify s1 :/: simplify s2
simplify (S s1)      = S (map head (words s1))
simplify (F _ s)     = s
simplify (Ref _ _)   = error "Attempting to simplify an existing reference"
simplify _           = Empty

writeSec :: Int -> Spec
writeSec n
  | n < 0     = error "Illegal section depth. Must be positive."
  | n == 0    = S "Sec:"
  | n == 1    = S "Subsec:"
  | otherwise = S $ (capitalize $ concat $ replicate n "sub") ++ "sec:"