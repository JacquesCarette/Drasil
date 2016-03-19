{-# OPTIONS -Wall #-}
module Reference where

import LayoutObjs
import Spec
import Data.Char
import Helpers

-- Creating References --
makeRef :: LayoutObj -> Spec
makeRef (Table h d t b)  = Ref (S "Table:" :+: simplify t)
makeRef (Section d t _)  = Ref (writeSec d :+: simplify t)
makeRef (Paragraph c)    = error "Can't reference paragraphs" --yet
makeRef (EqnBlock c)     = error "EqnBlock ref unimplemented"
makeRef (CodeBlock c)    = error "Codeblock ref unimplemented"
makeRef (Definition d)   = error "Definition ref unimplemented"
makeRef (BulletList b)   = error "BulletList ref unimplemented"
makeRef (NumberedList i) = error "NumberedList ref unimplemented"
makeRef (SimpleList p)   = error "SimpleList ref unimplemented"

simplify :: Spec -> Spec
simplify (s1 :-: s2) = simplify s1 :-: simplify s2
simplify (s1 :^: s2) = simplify s1 :^: simplify s2
simplify (s1 :+: s2) = simplify s1 :+: simplify s2
simplify (s1 :/: s2) = simplify s1 :/: simplify s2
simplify (S s1)      = S (filter (not . isSpace) s1)
simplify (F _ s)     = s
simplify (Ref _)     = error "Attempting to simplify an existing reference"
simplify x           = x

writeSec n
  | n < 0     = error "Illegal section depth. Must be positive."
  | n == 0    = S "Sec:"
  | n == 1    = S "Subsec:"
  | otherwise = S $ (capitalize $ concat $ replicate n "sub") ++ "sec:"