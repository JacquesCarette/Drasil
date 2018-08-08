module Drasil.DocLang.GenBuilders where

import Language.Drasil
import qualified Data.Drasil.Concepts.Documentation as Doc (introduction, reference, 
  traceyMandG, tOfSymb)

intro, reference, traceyMandG, tOfSymb :: [Contents] -> [Section] -> Section

intro       cs ss = section' (titleize Doc.introduction) cs ss "Intro"
reference   cs ss = section' (titleize' Doc.reference)   cs ss "References" --FIXME: label is available
traceyMandG cs ss = section' (titleize' Doc.traceyMandG) cs ss "TraceMatrices"
tOfSymb     cs ss = section' (titleize Doc.tOfSymb)      cs ss "ToS" --FIXME: label is available

--function that sets the shortname of each section to be the reference address
section' :: Sentence -> [Contents] -> [Section] -> RefAdd -> Section
section' a b c d = section a b c d (shortname' $ getStr a) --FIXME: getStr hack
  where
    getStr :: Sentence -> String
    getStr (S s) = s
    getStr ((:+:) s1 s2) = getStr s1 ++ getStr s2
    getStr _ = error "Term is not a string"
