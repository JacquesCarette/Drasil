module Drasil.Introduction 
  (introductionF,
   orgSec,
   introF,
   prpsOfDocF,
   scpOfReqF,
   charIntRdrF
   ) where

import Language.Drasil
import qualified Drasil.SRS as SRS
import Data.Drasil.SentenceStructures (foldlSent, ofThe, ofThe',
  foldlList, foldlsC, refineChain)
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Computation (algorithm)
import Control.Lens ((^.))

introductionF :: CI -> (Sentence, Sentence) -> Sentence -> (Sentence, Sentence) -> (Sentence, Sentence, Sentence) -> Bool -> (Sentence, CI, Section, Sentence) -> Section
introductionF kWord (startIntro, kSent) (pOdPart1) (inc, endSCOR) (know, und, appStandd) orgTrailing (i, b, s, t) 
  = introF startIntro kSent subsec
     where  subsec   = [pOfDoc, scpOfReq_, cIntRdr, organizationOfDoc orgTrailing]
            pOfDoc   = prpsOfDocF pOdPart1
            scpOfReq_ = scpOfReqF inc kWord endSCOR
            cIntRdr  = charIntRdrF know und kWord appStandd (SRS.userChar [] [])
            organizationOfDoc True  = orgSecWTS i b s t
            organizationOfDoc False = orgSec i b s 

--Provide the start to the intro, then the key sentence relating to the overview, and subsections
introF :: Sentence -> Sentence -> [Section] -> Section
introF start kSent subSec = SRS.intro [Paragraph start, Paragraph end] subSec
      where end = foldlSent [S "The following", fterm phrase section_,
                  S "provides an overview of the", introduceAbb srs,
                  S "for" +:+. kSent, S "This", fterm phrase section_, S "explains the", fterm phrase purpose,
                  S "of this", (fterm phrase document) `sC` foldlList (map ((\(x,y) -> x `ofThe` y)) (listOfIntroSubsec))]

--list is used by introF (current args passed in are the same for every example)
listOfIntroSubsec :: [(Sentence, Sentence)]
listOfIntroSubsec = [(fterm phrase scope, fterm phrase system), (fterm phrase organization, fterm phrase document), (fterm plural characteristic, fterm phrase intReader)]

-- provide only the first paragraph (as a sentence type) to 
prpsOfDocF :: Sentence -> Section
prpsOfDocF par1 = SRS.prpsOfDoc [Paragraph par1, Paragraph par2] []
      where par2 = foldlSent [S "This", fterm phrase document, 
                    S "will be used as a starting point for subsequent development", 
                    S "phases, including writing the", fterm phrase desSpec, S "and the", 
                    fterm phrase softwareVAV, S "plan. The", fterm phrase designDoc,
                    S "will show how the", fterm plural requirement, S "are to be realized, including",
                    fterm plural decision, S "on the numerical", (plural $ algorithm ^. term), 
                    S "and programming" +:+. fterm phrase environment, S "The", fterm phrase vavPlan, 
                    S "will show the steps that will be used to increase confidence in the",
                    fterm phrase softwareDoc, S "and the" +:+. fterm phrase implementation, S "Although",
                    S "the", short srs, S "fits in a series of", fterm plural document, 
                    S "that follow the so-called waterfall", (fterm phrase model) `sC` 
                    S "the actual development process is not constrained", 
                    S "in any way. Even when the waterfall model is not followed, as",
                    S "Parnas and Clements point out, the most logical way", --FIXME: add citation to these people?
                    S "to present the", fterm phrase documentation, S "is still to",
                    Quote (S "fake"), S "a rational", fterm phrase design, S "process"]

-- Complete the sentences, no need to add a period at the end of your input sentences
scpOfReqF :: Sentence -> CI -> Sentence -> Section
scpOfReqF includes progName ending = SRS.scpOfReq [Paragraph intro] []
  where intro = foldlSent [(fterm phrase scope) `ofThe'` (fterm plural requirement),
                S "includes" +:+. includes, S "Given appropriate inputs, the code for",
                short progName, S "is intended to" +:+ ending]

--Characteristics of Intended Reader section
charIntRdrF :: Sentence -> Sentence -> CI -> Sentence -> Section -> Section
charIntRdrF know und progName appStandd r = 
  SRS.charOfIR (intReaderIntro know und progName appStandd r) []

--paragraph called by charIntRdrF
intReaderIntro :: Sentence -> Sentence -> CI -> Sentence -> Section -> [Contents]
intReaderIntro know und progName appStandd r = [Paragraph $ foldlSent [S "Reviewers of this",
  (fterm phrase documentation), S "should have a strong knowledge in" +:+. know,
  S "The reviewers should also have an understanding of" +:+. und :+:
  appStandd, S "The", (fterm plural user), S "of", (short progName),
  S "can have a lower level of expertise, as explained in", (makeRef r)]]

-- | Organization of the document section builder. Takes an introduction,
-- a "bottom" chunk (where to start reading bottom-up. Usually instance
-- models or data definitions), a bottom section (for creating a reference link)
-- which should match the bottom chunk, but does not have to.
orgSec :: (NamedIdea c) => Sentence -> c -> Section -> Section
orgSec i b s = SRS.orgOfDoc (orgIntro i b s Nothing) []

-- | Same as 'orgSec' with the addition of extra information at the end 
-- (post-refine chain)?
orgSecWTS :: (NamedIdea c) => Sentence -> c -> Section -> Sentence -> Section
orgSecWTS i b s t = SRS.orgOfDoc (orgIntro i b s (Just t)) []

-- Intro -> Bottom (for bottom up approach) -> Section that contains bottom ->
--    trailing sentences -> [Contents]
orgIntro :: (NamedIdea c) => Sentence -> c -> Section -> Maybe Sentence -> [Contents]
orgIntro intro bottom bottomSec trailingSentence = [Paragraph $ foldlSent [
          intro, S "The presentation follows the standard pattern of presenting",
          (foldlsC $ map (fterm plural) [goal, theory, definition]) `sC` S "and assumptions.",
          S "For readers that would like a more bottom up approach" `sC`
          S "they can start reading the", fterm plural bottom, 
          S "in", makeRef bottomSec +:+
          S "and trace back to find any additional information they require"],
          Paragraph $ lastS trailingSentence]
          where lastS Nothing = refineChain [goalStmt, thModel, inModel]
                lastS (Just t) = lastS Nothing +:+. t