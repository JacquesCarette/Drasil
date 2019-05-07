module Drasil.Sections.Introduction (orgSec, introductionSection, purposeOfDoc, scopeOfRequirements, 
  charIntRdrF) where

import Language.Drasil
import qualified Drasil.DocLang.SRS as SRS (intro, prpsOfDoc, scpOfReq, charOfIR, orgOfDoc, goalStmt, thModel, inModel)

import Data.Drasil.Concepts.Computation (algorithm)
import Data.Drasil.Concepts.Documentation as Doc (goal, organization, thModel, inModel, goalStmt,
  documentation, user, theory, definition, scope, requirement, section_, document, purpose,
  system, model, design, intReader, srs, characteristic, designDoc, decision, environment,
  vavPlan, softwareDoc, implementation, softwareVAV, desSpec)
import Data.Drasil.Citations (parnasClements1986)
import Data.Drasil.SentenceStructures (FoldType(List), SepType(Comma),
  foldlList, foldlsC, foldlSP, ofThe, ofThe', refineChain)

-----------------------
--     Constants     --
-----------------------

-- | Contents explaining the development process of this program
developmentProcessParagraph :: Contents
developmentProcessParagraph = foldlSP [S "This", phrase document, 
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
  S "Parnas and Clements point out", makeCiteS parnasClements1986 `sC`
  S "the most logical way to present the", phrase documentation,
  S "is still to", Quote (S "fake"), S "a rational", phrase design,
  S "process"]

-- | Sentence containing the subsections of the introduction
introductionSubsections :: Sentence
introductionSubsections = foldlList Comma List (map (uncurry ofThe) 
  [(phrase scope, phrase system), 
  (plural characteristic, phrase intReader),
  (phrase Doc.organization, phrase document)])

-------------------------
--                    --
-------------------------

-- | Constructor for the introduction section
-- problemIntroduction - Sentence introducing the specific example problem
-- programDefinition  - Sentence definition of the specific example
-- subSections        - List of subsections for this section
introductionSection :: Sentence -> Sentence -> [Section] -> Section
introductionSection problemIntroduction programDefinition = SRS.intro 
  [mkParagraph problemIntroduction, (overviewParagraph programDefinition)]


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
purposeOfDoc :: Sentence -> Section
purposeOfDoc purposeOfProgramParagraph = SRS.prpsOfDoc 
  [mkParagraph purposeOfProgramParagraph, developmentProcessParagraph] []


-- | constructor for scope of requirements subsection
-- mainRequirement  - the main requirement for the program
-- programName      - the name of the program
-- intendedPurpose  - the intended purpose of the program
scopeOfRequirements :: Idea a => Sentence -> a -> Sentence -> Section
scopeOfRequirements mainRequirement _ EmptyS = SRS.scpOfReq [scpBody] []
  where scpBody = foldlSP [(phrase scope) `ofThe'` (plural requirement),
                  S "includes", mainRequirement]
scopeOfRequirements mainRequirement programName intendedPurpose = SRS.scpOfReq [scpBody] []
  where scpBody = foldlSP [(phrase scope) `ofThe'` (plural requirement),
                  S "includes" +:+. mainRequirement, S "Given the appropriate",
                  S "inputs" `sC` short programName +:+ intendedPurpose]

-- | constructor for characteristics of the intended reader subsection
-- progName
-- assumed
-- topic
-- asset
-- r
charIntRdrF :: (Idea a) => a -> [Sentence] -> [Sentence] -> [Sentence] -> 
  Section -> Section
charIntRdrF progName assumed topic asset r = 
  SRS.charOfIR (intReaderIntro progName assumed topic asset r) []

--paragraph called by charIntRdrF
-- assumed    - subjects the reader is assumed to understand
-- topic      - topic-related subjects that the reader should understand
-- asset      - subjects that would be an asset if the reader understood them
-- sectionRef - reference to user characteristic section
intReaderIntro :: (Idea a) => a -> [Sentence] -> [Sentence] -> [Sentence] ->
  Section -> [Contents]
intReaderIntro progName assumed topic [] sectionRef = 
  [foldlSP [S "Reviewers of this", (phrase documentation), 
  S "should have an understanding of" +:+. 
  foldlList Comma List (assumed ++ topic), S "The", (plural user), 
  S "of", (short progName), S "can have a lower level of expertise, as", 
  S "explained in", (makeRef2S sectionRef)]]
intReaderIntro progName assumed topic asset sectionRef = 
  [foldlSP [S "Reviewers of this", (phrase documentation), 
  S "should have an understanding of" +:+. 
  foldlList Comma List (assumed ++ topic), S "It would be an asset to",
  S "understand" +:+. foldlList Comma List asset, S "The", (plural user), 
  S "of", (short progName), S "can have a lower level of expertise, as", 
  S "explained in", (makeRef2S sectionRef)]]

-- | Doc.organization of the document section constructor.  => Sentence -> c -> Section -> Sentence -> Section
orgSec :: NamedIdea c => Sentence -> c -> Section -> Sentence -> Section
orgSec i b s t = SRS.orgOfDoc (orgIntro i b s t) []

-- Intro -> Bottom (for bottom up approach) -> Section that contains bottom ->
--    trailing sentences -> [Contents]
orgIntro :: NamedIdea c => Sentence -> c -> Section -> Sentence -> [Contents]
orgIntro intro bottom bottomSec trailingSentence = [foldlSP [
          intro, S "The presentation follows the standard pattern of presenting",
          (foldlsC $ map plural [Doc.goal, theory, definition]) `sC` S "and assumptions.",
          S "For readers that would like a more bottom up approach" `sC`
          S "they can start reading the", plural bottom,
          S "in", makeRef2S bottomSec +:+
          S "and trace back to find any additional information they require"],
          mkParagraph $ lastS trailingSentence]
          where lastS EmptyS = refineChain $ zip [goalStmt, thModel, inModel]
                  [SRS.goalStmt [] [], SRS.thModel [] [], SRS.inModel [] []]
                lastS t = refineChain (zip [goalStmt, thModel, inModel] 
                  [SRS.goalStmt [] [], SRS.thModel [] [], SRS.inModel [] []]) 
                  +:+. t
