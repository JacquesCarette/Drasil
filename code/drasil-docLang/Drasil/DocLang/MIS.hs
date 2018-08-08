module Drasil.DocLang.MIS where

import Language.Drasil
import Drasil.DocLang.GenBuilders (section')

import qualified Data.Drasil.Concepts.Documentation as Doc (misOfModule, moduleHierarchy, notation)
import Data.Drasil.SentenceStructures (foldlSP)

modHier, notation :: [Contents] -> [Section] -> Section

modHier  cs ss = section' (titleize Doc.moduleHierarchy)  cs ss "ModHierarchy"
notation cs ss = section' (titleize Doc.notation)         cs ss "Notation"

misOfModule :: [Contents] -> [Section] -> String -> Section
misOfModule cs ss mod = section' (titleize $ Doc.misOfModule mod)  cs ss $ "MISof" ++ mod ++ "Module"


--FIXME: needs variability
introMIS :: Contents
introMIS = foldlSP [S "The following document details the Module Interface Specifications", 
  S "for the implemented modules in the program GlassBR. It is intended to ease",
  S " navigation through the program for design and maintenance purposes. ",
  S "Complementary documents include the System Requirement Specifications (SRS)", 
  S " and Module Guide (MG). The full documentation and implementation can be found",
  S " at https://github.com/smiths/caseStudies/tree/master/CaseStudies/glass."]

{-notationIntroMIS :: Contents
notationIntroMIS = The structure of the MIS for modules comes from Homan and Strooper (1995), with the
addition that template modules have been adapted from Ghezzi et al. (2003). The mathe-
matical notation comes from Chapter 3 of Homan and Strooper (1995). For instance, the
symbol := is used for a multiple assignment statement and conditional rules follow the form
(c1 ) r1jc2 ) r2j:::jcn ) rn).
The following table summarizes the primitive data types used by GlassBR.-}