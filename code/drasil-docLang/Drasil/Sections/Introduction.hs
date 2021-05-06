module Drasil.Sections.Introduction (orgSec, introductionSection, purposeOfDoc, scopeOfRequirements, 
  charIntRdrF, purpDoc) where

import Language.Drasil
import qualified Drasil.DocLang.SRS as SRS (intro, prpsOfDoc, scpOfReq,
  charOfIR, orgOfDoc, goalStmt, thModel, inModel, sysCon)
import Drasil.DocumentLanguage.Definitions(Verbosity(..))
import Utils.Drasil

import Data.Drasil.Concepts.Computation (algorithm)
import Data.Drasil.Concepts.Documentation as Doc (assumption, characteristic,
  decision, definition, desSpec, design, designDoc, document, documentation,
  environment, goal, goalStmt, implementation, intReader, model, organization,
  purpose, requirement, scope, section_, softwareDoc, softwareVAV, srs,
  theory, user, vavPlan, problem, information, systemConstraint)
import Data.Drasil.TheoryConcepts as Doc (inModel, thModel)
import Data.Drasil.Citations (parnasClements1986)


-----------------------
--     Constants     --
-----------------------

-- | Contents explaining the development process of this program
developmentProcessParagraph :: Sentence
developmentProcessParagraph = foldlSent [S "This", phrase document, 
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
introductionSubsections = foldlList Comma List (map (uncurry the_ofThe) 
  [(phrase scope, plural requirement), 
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
  [mkParagraph problemIntroduction, overviewParagraph programDefinition]


-- | Constructor for the overview paragraph for the introduction
-- programDefinition - defintion of the specific example being generated
overviewParagraph :: Sentence -> Contents
overviewParagraph programDefinition = foldlSP [S "The following", phrase section_,
  S "provides an overview of the", introduceAbb srs, S "for" +:+. 
  programDefinition, S "This", phrase section_, S "explains the", phrase purpose,
  S "of this", phrase document `sC` introductionSubsections]


-- | Constructor for purpose of document function that each example controls
-- | verbosity controls if second paragraph is added or not
purpDocPara1 :: CI -> Sentence 
purpDocPara1 proName = foldlSent [S "The primary purpose of this", phrase document, S "is to",
  S "record the", plural requirement, S "of the" +:+. titleize proName, 
  atStart' goal `sC` plural assumption `sC` plural thModel `sC` 
  plural definition `sC` S "and other", phrase model, S "derivation",
  phrase information, S "are specified" `sC` S "allowing the reader to fully",
  S "understand" `sAnd` S "verify the", phrase purpose `sAnd` S "scientific",
  S "basis of" +:+. short proName, S "With the exception of", 
  plural systemConstraint, S "in", makeRef2S (SRS.sysCon [] []) `sC` S "this",
  short Doc.srs, S "will remain abstract, describing what", phrase problem,
  S "is being solved, but not how to solve it"] 
purpDoc :: CI->Verbosity -> [Sentence]
purpDoc proName Verbose = [purpDocPara1 proName, developmentProcessParagraph]
purpDoc proName Succinct = [purpDocPara1 proName]
-- | constructor for purpose of document subsection
-- purposeOfProgramParagraph - a sentence explaining the purpose of the specific 
-- example
purposeOfDoc :: [Sentence] -> Section
purposeOfDoc [purposeOfProgram] = SRS.prpsOfDoc [mkParagraph purposeOfProgram] []
purposeOfDoc [purposeOfProgram, developmentProcess] = SRS.prpsOfDoc 
  [mkParagraph purposeOfProgram, mkParagraph developmentProcess] []
purposeOfDoc _ = SRS.prpsOfDoc [mkParagraph developmentProcessParagraph] []

-- | constructor for scope of requirements subsection
-- req - the main requirement for the program
scopeOfRequirements :: Sentence -> Section
scopeOfRequirements req = SRS.scpOfReq [foldlSP
  [phrase scope `the_ofThe'` plural requirement, S "includes", req]] []

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
intReaderIntro progName assumed topic asset sectionRef = 
  [foldlSP [S "Reviewers of this", phrase documentation,
  S "should have an understanding of" +:+.
  foldlList Comma List (assumed ++ topic), assetSent, S "The",
  plural user `sOf` short progName, S "can have a lower level" `sOf`
  S "expertise, as explained" `sIn` makeRef2S sectionRef]]
  where
    assetSent = case asset of
      [] -> EmptyS
      _  -> S "It would be an asset to understand" +:+. foldlList Comma List asset

-- | Doc.organization of the document section constructor.  => Sentence -> c -> Section -> Sentence -> Section
orgSec :: NamedIdea c => Sentence -> c -> Section -> Sentence -> Section
orgSec i b s t = SRS.orgOfDoc (orgIntro i b s t) []

-- Intro -> Bottom (for bottom up approach) -> Section that contains bottom ->
--    trailing sentences -> [Contents]
orgIntro :: NamedIdea c => Sentence -> c -> Section -> Sentence -> [Contents]
orgIntro intro bottom bottomSec trailingSentence = [foldlSP [
  intro, S "The presentation follows the standard pattern of presenting" +:+.
  foldlList Comma List (map plural [nw Doc.goal, nw theory, nw definition, nw assumption]),
  S "For readers that would like a more bottom up approach" `sC`
  S "they can start reading the", plural bottom `sIn` makeRef2S bottomSec `sAnd`
  S "trace back to find any additional information they require"],
  folder [refineChain (zip [goalStmt, thModel, inModel]
         [SRS.goalStmt [] [], SRS.thModel [] [], SRS.inModel [] []]), trailingSentence]]
  where
    folder = case trailingSentence of
      EmptyS -> foldlSP_
      _      -> foldlSP
