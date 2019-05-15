module Drasil.DocLang.GenBuilders where

import Language.Drasil
import qualified Data.Drasil.Concepts.Documentation as Doc (introduction, reference, 
  traceyMandG, tOfSymb, assumption)

intro, reference, traceyMandG, tOfSymb, assumpt :: [Contents] -> [Section] -> Section

intro       cs ss = section' (titleize  Doc.introduction) cs ss "Intro"
traceyMandG cs ss = section' (titleize' Doc.traceyMandG)  cs ss "TraceMatrices"

assumpt   cs ss = section (titleize' Doc.assumption) cs ss assumptLabel
reference cs ss = section (titleize' Doc.reference)  cs ss referenceLabel
tOfSymb   cs ss = section (titleize  Doc.tOfSymb)    cs ss tOfSymbLabel

assumptLabel, referenceLabel, tOfSymbLabel :: Reference
assumptLabel   = makeSecRef "Assumps"    "Assumptions"
referenceLabel = makeSecRef "References" "References" 
tOfSymbLabel   = makeSecRef "ToS"        "Table of Symbols"

--function that sets the shortname of each section to be the reference address
section' :: Sentence -> [Contents] -> [Section] -> String -> Section
section' a b c d = section a b c (makeSecRef d (toString a))
  where
    toString :: Sentence -> String --FIXME: same as getStr hack, import instead? 
    toString (S x) = x
    toString ((:+:) s1 s2) = toString s1 ++ toString s2
    toString _ = error "Term is not a string"