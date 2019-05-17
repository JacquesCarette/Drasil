module Drasil.DocLang.MIS where

import Language.Drasil
import Drasil.DocLang.GenBuilders (section')

import qualified Data.Drasil.Concepts.Documentation as Doc (accRoutSemantic, 
  consideration, enviroVar, expConstant, expType, misOfModule, 
  module_, moduleHierarchy, notation, semantic, stateInvar, stateVar, syntax, use,
  templateModule)
import qualified Data.Drasil.Concepts.Software as Doc (expAccProgram)
  -- import ^ for section making

import Data.Drasil.Concepts.Computation (dataType)
import Data.Drasil.Concepts.Documentation (description, design, document,
  documentation, form, implementation, mg, mis, purpose, srs, statement,
  symbol_, templateModule)
import Data.Drasil.Concepts.Software (program)
  -- import ^ for paragraphs (pull out)

import Data.Drasil.People (carloGhezzi, mJazayeri, dMandrioli, hoffman, strooper)
import Data.Drasil.SentenceStructures (foldlSP, inThe, ofThe', sAnd)

import Control.Lens ((^.))

accRoutSemantics, considerations, enviroVars, expAccPrograms, 
  expConstants, expTypes, module_, tempMod_, modHier, notation, semantics, stateInvars, 
  stateVars, syntax, uses :: [Contents] -> [Section] -> Section

modHier          cs ss = section' (titleize  Doc.moduleHierarchy) cs ss "ModHierarchy"
notation         cs ss = section' (titleize  Doc.notation)        cs ss "Notation"

module_          cs ss = section' (titleize  Doc.module_)         cs ss "Module"
tempMod_         cs ss = section' (titleize  Doc.templateModule)  cs ss "TemplateModule"
uses             cs ss = section' (titleize' Doc.use)             cs ss "Uses"
syntax           cs ss = section' (titleize  Doc.syntax)          cs ss "Syntax"
semantics        cs ss = section' (titleize' Doc.semantic)        cs ss "Semantics"
considerations   cs ss = section' (titleize' Doc.consideration)   cs ss "Considerations"

expConstants     cs ss = section' (titleize' Doc.expConstant)     cs ss "ExpConstants"
expTypes         cs ss = section' (titleize' Doc.expType)         cs ss "ExpTypes"
expAccPrograms   cs ss = section' (titleize' Doc.expAccProgram)   cs ss "ExpAccPrograms"

enviroVars       cs ss = section' (titleize' Doc.enviroVar)       cs ss "EnviroVars"
stateVars        cs ss = section' (titleize' Doc.stateVar)        cs ss "StateVars"
stateInvars      cs ss = section' (titleize' Doc.stateInvar)      cs ss "StateInvars"
accRoutSemantics cs ss = section' (titleize' Doc.accRoutSemantic) cs ss "AccRoutSemantics"

misOfModule :: [Contents] -> [Section] -> String -> Reference -> Section
misOfModule cs ss modstr = section (titleize $ Doc.misOfModule modstr) cs ss

--FIXME: All these contents need variability to be implemented in other examples

------------------
-- INTRODUCTION --
------------------

introMIS :: (Idea a) => a -> Sentence -> Contents
introMIS progName outerLink = foldlSP [S "The following", phrase document, S "details the", 
  titleize mis, sParen (getAcc mis), S "for the implemented", plural Doc.module_ `inThe`
  phrase program, short progName :+: S ". It is intended to ease navigation through the",
  phrase program, S "for", phrase design, S "and maintenance" +:+. plural purpose,
  S "Complementary", plural document, S "include the", titleize srs, (sParen $ getAcc srs)
  `sAnd` titleize mg +:+. sParen (getAcc mg), S "The full", phrase documentation `sAnd`
  phrase implementation, S "can be found at", outerLink]
--FIXME: outerLink needs to work!

--------------
-- NOTATION --
--------------

misCitations :: BibRef
misCitations = [hoffmanAndStrooper1995, ghezziEtAl2003]

hoffmanAndStrooper1995, ghezziEtAl2003 :: Citation

ghezziEtAl2003 = cMisc
  [author [carloGhezzi, mJazayeri, dMandrioli],
  title "Fundamentals of Software Engineering",
  year 2003,
  publisher "Prentice Hall",
  address "Upper Saddle River, NJ, USA",
  edition 2]
  "ghezziEtAl2003"

hoffmanAndStrooper1995 = cMisc
  [author [hoffman, strooper],
  title "Software Design, Automated Testing, and Maintenance: A Practical Approach",
  publisher "International Thoomson Computer Press",
  address "New York, NY, USA",
  year 1995,
  howPublishedU "http://citeseer.ist.psu.edu/428727.html"]
  "hoffmanAndStrooper1995"

notationIntroMIS :: Contents
notationIntroMIS = foldlSP [S "structure" `ofThe'` getAcc mis, S "for", plural Doc.module_,
  S "comes from", makeRef2S hoffmanAndStrooper1995 `sC` S "with the addition that", plural templateModule, 
  S "have been adapted from" +:+. makeRef2S ghezziEtAl2003, S"The mathematical", phrase Doc.notation, 
  S "comes from Chapter 3 of" +:+. makeRef2S hoffmanAndStrooper1995, S "For instance, the", phrase symbol_, 
  S ":= is used for a multiple assignment", phrase statement `sAnd` S "conditional rules",
  S "follow the", phrase form, S "(c1 => r1 | c2 => r2 | ... | cn => rn)"] -- FIXME: Hardcoded expression

notTblIntro :: (Idea a) => a -> Contents
notTblIntro progName = mkParagraph $ S "The following table summarizes the primitive" +:+
  (plural dataType) +:+ S "used by" +: (short progName)

notationTable :: [DefinedQuantityDict] -> Contents
notationTable n = LlC $ llcc (makeTabRef "ToN") $ Table
  (map (at_start) [dataType, Doc.notation, description]) (mkTable
  [phrase,
   (\x -> P $ symbol x Implementation),
   (\x -> (x ^. defn))
  ] n)
  (S "Table of Notation") False

notationIntroContd :: (Idea a) => a -> Contents
notationIntroContd progName = foldlSP [S "The specification of", short progName, 
  S "uses some derived data types: sequences, strings, and tuples.", 
  S " Sequences are lists that represent a countable number",
  S " of ordered values of the same data type, where the same value may occur more than once.",
  S " Strings are sequences of characters. Tuples contain a list of values, potentially of ", 
  S "different types. In addition, ", short progName, S "uses functions, which are defined by the data types",
  S " of their inputs and outputs. Local functions are described by giving their type signature",
  S " followed by their specification"]

----------------------
-- MODULE HIERARCHY --
----------------------

modHierarchyPointer :: Sentence -> Contents
modHierarchyPointer mgLink = mkParagraph $ S "To view the Module Hierarchy, see" +:+ mgLink

-------------------
-- MODULE INTROS --
-------------------

hwModIntro :: Contents
hwModIntro = foldlSP [S "This module hides the underlying hardware for I/O (to the",
  S "screen, or file, or other device). In general it will be provided by the selected",
  S "programming language and operating system"]

inputModIntro :: Contents
inputModIntro = foldlSP [S "The secrets of this module are the data structure for input",
  S "parameters, how the values are input and how the values are verified. The load and",
  S "verify secrets are isolated to their own access programs. This module follows the",
  S "singleton pattern; that is, there is only one instance of this module"]

-----------------------
-- HELPFUL FUNCTIONS --
-----------------------

assignSttmts :: (HasUID c, HasSymbol c, DefiningExpr c) => c -> Sentence
assignSttmts f = (ch f) :+: S ":=" :+: E (f ^. defnExpr) --FIXME: replace ":=" with actual symbol ":="

{-WIP : laying out the access routine semantics
layAccRoutSemantics :: (HasUID c, HasSymbol c, ExprRelat c) => String -> [c] ->
  [Sentence] -> Maybe Sentence -> [Contents]
layAccRoutSemantics fxnName inputs transitionContents exception = (UlC $ ulcc $
  Enumeration $ Simple $ 
  [ S fxnName +:+ sParen(map makeRef inputs)
  , Nested (S "transition") $ Bullet $ noRefs $
    (map Flat transitionContents)
  , Nothing]) ++ (mkParagraph $ getExceptionSttmt exception)
  where
    getExceptionSttmt :: Maybe Sentence -> Sentence
    getExceptionSttmt Nothing  = S "exception: None" 
    getExceptionSttmt (Just x) = S "exception: " +:+ x-}
