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


--FIXME: All these contents need variability to be implemented in other examples

------------------
-- INTRODUCTION --
------------------

introMIS :: Contents
introMIS = foldlSP [S "The following document details the Module Interface Specifications", 
  S "for the implemented modules in the program GlassBR. It is intended to ease",
  S " navigation through the program for design and maintenance purposes. ",
  S "Complementary documents include the System Requirement Specifications (SRS)", 
  S " and Module Guide (MG). The full documentation and implementation can be found",
  S " at https://github.com/smiths/caseStudies/tree/master/CaseStudies/glass"]

--------------
-- NOTATION --
--------------

notationIntroMIS :: Contents
notationIntroMIS = foldlSP [S "The structure of the MIS for modules comes from ",
  S "Hoffman and Strooper (1995), with the addition that template modules have been:",
  S " adapted from Ghezzi et al. (2003). The mathematical notation comes from ",
  S "Chapter 3 of Hoffman and Strooper (1995). For instance, the symbol := is used",
  S " for a multiple assignment statement and conditional rules follow the form",
  S "(c1 ) r1jc2 ) r2j:::jcn ) rn)"]

notTblIntro :: Contents
notTblIntro = mkParagraph $ S "The following table summarizes the primitive" +:+
  S "data types used by GlassBR"

--notationTable :: LabelledContent
--notationTable 

notationIntroContd :: Contents
notationIntroContd = foldlSP [S "The specification of GlassBR uses some derived data types:",
  S " sequences, strings, and tuples. Sequences are lists that represent a countable number",
  S " of ordered values of the same data type, where the same value may occur more than once.",
  S " Strings are sequences of characters. Tuples contain a list of values, potentially of ", 
  S "different types. In addition, GlassBR uses functions, which are defined by the data types",
  S " of their inputs and outputs. Local functions are described by giving their type signature",
  S " followed by their specification"]

----------------------
-- MODULE HIERARCHY --
----------------------

modHierarchyPointer :: Contents
modHierarchyPointer = mkParagraph $ S "To view the Module Hierarchy, see section 3 of the MG (Link)."
