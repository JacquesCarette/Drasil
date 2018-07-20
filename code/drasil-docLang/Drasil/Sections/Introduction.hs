module Drasil.Sections.Introduction 
  (orgSec,
   introductionSection,
   purposeOfDoc,
   scopeOfRequirements,
   charIntRdrF
   ) where

import Language.Drasil
import qualified Drasil.DocLang.SRS as SRS (intro, prpsOfDoc, scpOfReq, charOfIR, orgOfDoc)
import Data.Drasil.SentenceStructures (ofThe, ofThe',
  foldlList, foldlsC, refineChain, foldlSP)
import Data.Drasil.Concepts.Documentation as Doc (goal, organization, thModel, inModel, goalStmt,
  documentation, user, theory, definition, scope, requirement, section_, document, purpose,
  system, model, design, intReader, srs, characteristic, designDoc, decision, environment,
  vavPlan, softwareDoc, implementation, softwareVAV, desSpec)
import Data.Drasil.Concepts.Computation (algorithm)
import Data.Drasil.Citations (parnasClements1986)
import Drasil.DocumentLanguage.RefHelpers (cite)

-----------------------
--     Constants     --
-----------------------

-- | Contents explaining the development process of this program
developmentProcessParagraph :: ReferenceDB -> Contents
developmentProcessParagraph refdb = foldlSP [S "This", phrase document, 
  S "will be used as a starting point for subsequent development", 
  S "phases, including writing the", phrase desSpec, S "and the", 
  phrase softwareVAV, S "plan. The", phrase designDoc, S "will show how the", 
  plural requirement, S "are to be realized, including", plural decision, 
  S "on the numerical", plural algorithm, S "and programming" +:+. 
  phrase environment, S "The", phrase vavPlan, 
  S "will show the steps that will be used to increase confidence in the",
  phrase softwareDoc, S "and the" +:+. phrase implementation, S "Although",
  S "the", short srs, S "fits in a series of", plural document, 
  S "that follow the so-called waterfall", phrase model `sC` 
  S "the actual development process is not constrained", 
  S "in any way. Even when the waterfall model is not followed, as",
  S "Parnas and Clements point out",  cite refdb parnasClements1986 `sC`
  S "the most logical way to present the", phrase documentation,
  S "is still to", Quote (S "fake"), S "a rational", phrase design,
  S "process"]

-- | Sentence containing the subsections of the introduction
introductionSubsections :: Sentence
introductionSubsections = foldlList (map (\(x,y) -> x `ofThe` y) 
  [(phrase scope, phrase system), 
  (phrase Doc.organization, phrase document), 
  (plural characteristic, phrase intReader)])

-------------------------
--                    --
-------------------------

-- | Constructor for the introduction section
-- problemIntroduction - Sentence introducing the specific example problem
-- programDefinition  - Sentence definition of the specific example
-- subSections        - List of subsections for this section
introductionSection :: Sentence -> Sentence -> [Section] -> Section
introductionSection problemIntroduction programDefinition subSections = SRS.intro 
  [Paragraph problemIntroduction, (overviewParagraph programDefinition)] subSections


-- | Constructor for the overview paragraph for the introduction
-- programDefinition - defintion of the specific example being generated
overviewParagraph :: Sentence -> Contents
overviewParagraph programDefinition = foldlSP [S "The following", phrase section_,
  S "provides an overview of the", introduceAbb srs, S "for" +:+. 
  programDefinition, S "This", phrase section_, S "explains the", phrase purpose,
  S "of this", (phrase document) `sC` introductionSubsections]

-- | constructor for purpose of document subsection
-- purposeOfProgramParagraph - a sentence explaining the purpose of the specific 
-- example
purposeOfDoc :: ReferenceDB -> Sentence -> Section
purposeOfDoc refdb purposeOfProgramParagraph = SRS.prpsOfDoc 
  [Paragraph purposeOfProgramParagraph, developmentProcessParagraph refdb] []


-- | constructor for scope of requirements subsection
-- mainRequirement  - the main requirement for the program
-- programName      - the name of the program
-- intendedPurpose  - the intended purpose of the program
scopeOfRequirements :: (Idea a, CommonIdea a) => Sentence -> a -> Sentence -> Section
scopeOfRequirements mainRequirement programName intendedPurpose = SRS.scpOfReq [intro] []
  where intro = foldlSP [(phrase scope) `ofThe'` (plural requirement),
                S "includes" +:+. mainRequirement, S "Given the appropriate inputs,",
                short programName +:+ intendedPurpose]

-- | constructor for characteristics of the intended reader subsection
-- know
-- und
-- progName
-- appStandd
-- r
charIntRdrF :: (Idea a) => 
  Sentence -> Sentence -> a -> Sentence -> Section -> Section
charIntRdrF know und progName appStandd r = 
  SRS.charOfIR (intReaderIntro know und progName appStandd r) []

--paragraph called by charIntRdrF
-- topic1     - sentence the reader should have knowledge in
-- topic2     - sentence the reader should understand
-- stdrd      - sentence of the standards the reader should be familiar with
-- sectionRef - reference to user characteristic section
intReaderIntro :: (Idea a) => 
  Sentence -> Sentence -> a -> Sentence -> Section -> [Contents]
intReaderIntro EmptyS topic2 progName stdrd sectionRef = 
  [foldlSP [S "Reviewers of this",
  (phrase documentation), S "should have an understanding of" +:+. topic2 :+:
  stdrd, S "The", (plural user), S "of", (short progName),
  S "can have a lower level of expertise, as explained in", (makeRef sectionRef)]]
intReaderIntro topic1 topic2 progName stdrd sectionRef = 
  [foldlSP [S "Reviewers of this",
  (phrase documentation), S "should have a strong knowledge in" +:+. topic1,
  S "The reviewers should also have an understanding of" +:+. topic2 :+:
  stdrd, S "The", (plural user), S "of", (short progName),
  S "can have a lower level of expertise, as explained in", (makeRef sectionRef)]]

-- | Doc.organization of the document section constructor.  => Sentence -> c -> Section -> Sentence -> Section
orgSec :: NamedIdea c => Sentence -> c -> Section -> Sentence -> Section
orgSec i b s t = SRS.orgOfDoc (orgIntro i b s t) []

-- Intro -> Bottom (for bottom up approach) -> Section that contains bottom ->
--    trailing sentences -> [Contents]
orgIntro :: (NamedIdea c) => Sentence -> c -> Section -> Sentence -> [Contents]
orgIntro intro bottom bottomSec trailingSentence = [foldlSP [
          intro, S "The presentation follows the standard pattern of presenting",
          (foldlsC $ map (plural) [Doc.goal, theory, definition]) `sC` S "and assumptions.",
          S "For readers that would like a more bottom up approach" `sC`
          S "they can start reading the", plural bottom, 
          S "in", makeRef bottomSec +:+
          S "and trace back to find any additional information they require"],
          Paragraph $ lastS trailingSentence]
          where lastS EmptyS = refineChain [goalStmt, thModel, inModel]
                lastS t = refineChain [goalStmt, thModel, inModel] +:+. t
