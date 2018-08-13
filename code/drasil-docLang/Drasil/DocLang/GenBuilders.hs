module Drasil.DocLang.GenBuilders where

import Language.Drasil
import qualified Data.Drasil.Concepts.Documentation as Doc (introduction, reference, 
  traceyMandG, tOfSymb, assumption)

intro, reference, traceyMandG, tOfSymb, assumpt :: [Contents] -> [Section] -> Section

intro       cs ss = section' (titleize Doc.introduction) cs ss "Intro"
reference   cs ss = section' (titleize' Doc.reference)   cs ss "References" --FIXME: label is available
traceyMandG cs ss = section' (titleize' Doc.traceyMandG) cs ss "TraceMatrices"
tOfSymb     cs ss = section' (titleize Doc.tOfSymb)      cs ss "ToS" --FIXME: label is available
assumpt     cs ss = section (titleize' Doc.assumption)        cs ss assumptLabel

assumptLabel :: Label
assumptLabel = mkLabelRASec "Assumps" "Assumptions"

--function that sets the label of each section to be the reference address
section' :: Sentence -> [Contents] -> [Section] -> String -> Section
section' a b c d = section a b c (mkLabelRASec d (toString a))
  where
    toString :: Sentence -> String --FIXME: same as getStr hack
    toString (S x) = x
    toString ((:+:) s1 s2) = toString s1 ++ toString s2
    toString _ = error "Term is not a string"