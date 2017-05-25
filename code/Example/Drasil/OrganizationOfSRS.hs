module Drasil.OrganizationOfSRS (refineChain, orgSec, orgSecWTS, genSysF, 
                                 specSysDesF, datConF, datConPar,
                                 figureLabel, showingCxnBw, inModelF) where

import Language.Drasil
import Control.Lens ((^.))
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Math (ode)
import Data.Drasil.Utils (foldlsC)
import qualified Drasil.SRS as SRS

-- | Create a list in the pattern of "The __ are refined to the __".
-- Note: Order matters!
refineChain :: NamedIdea c => [c] -> Sentence
refineChain (x:y:[]) = S "The" +:+ word x +:+ S "are refined to the" +:+ word y
refineChain (x:y:xs) = refineChain [x,y] `sC` rc ([y] ++ xs)
refineChain _ = error "refineChain encountered an unexpected empty list"

-- | Helper used by refineChain
word :: NamedIdea c => c -> Sentence
word w = plural $ w ^. term

-- | Helper used by refineChain
rc :: NamedIdea c => [c] -> Sentence
rc (x:y:[]) = S "and the" +:+ (plural $ x ^. term) +:+ S "to the" +:+. 
  (plural $ y ^. term)
rc (x:y:xs) = S "the" +:+ word x +:+ S "to the" +:+ word y `sC` rc ([y] ++ xs)
rc _ = error "refineChain helper encountered an unexpected empty list"

figureLabel :: [Char] -> NPNC -> Sentence -> [Char]-> Contents
figureLabel num traceyMG contents filePath = Figure (titleize figure +: S num
  +:+ (showingCxnBw (traceyMG) (contents))) filePath

showingCxnBw :: NPNC -> Sentence -> Sentence
showingCxnBw traceyMG contents = titleize traceyMG +:+ S "Showing the" +:+ titleize' connection +:+
  S "Between" +:+ contents

-- | Organization of the document section builder. Takes an introduction,
-- a "bottom" chunk (where to start reading bottom-up. Usually instance
-- models or data definitions), a bottom section (for creating a reference link)
-- which should match the bottom chunk, but does not have to.
orgSec :: (NounPhrase c) => Sentence -> c -> Section -> Section
orgSec = \i b s ->
  Section (titleize orgOfDoc) (map Con (orgIntro i b s Nothing))

-- | Same as 'orgSec' with the addition of extra information at the end 
-- (post-refine chain)?
orgSecWTS :: (NounPhrase c) => Sentence -> c -> Section -> Sentence -> Section
orgSecWTS = \i b s t ->
  Section (titleize orgOfDoc) (map Con (orgIntro i b s (Just t)))
  
  
-- Intro -> Bottom (for bottom up approach) -> Section that contains bottom ->
--    trailing sentences -> [Contents]
orgIntro :: (NounPhrase c) => Sentence -> c -> Section -> Maybe Sentence -> [Contents]
orgIntro intro bottom bottomSec trailingSentence = [ Paragraph $
  intro +:+ S "The presentation follows the standard pattern of presenting" +:+
  (foldlsC $ map plural [goal, theory, definition]) `sC` S "and assumptions." +:+
  S "For readers that would like a more bottom up approach" `sC`
  S "they can start reading the" +:+ (plural bottom) +:+ 
  S "in" +:+ (makeRef bottomSec) +:+. 
  S "and trace back to find any additional information they require",
  Paragraph $ lastS trailingSentence ]
  where lastS Nothing = refineChain [goalStmt, thModel, inModel]
        lastS (Just t) = lastS Nothing +:+. t
       
-- wrapper for general system description
genSysF :: [Section] -> Section
genSysF = SRS.genSysDec [genSysIntro] 

--generalized general system description introduction
genSysIntro :: Contents
genSysIntro = Paragraph $ S "This" +:+ (phrase section_) +:+ S "provides general" +:+
  (phrase information) +:+ S "about the" +:+ (phrase system) `sC` S "identifies" +:+
  S "the interfaces between the" +:+ (phrase system) +:+ S "and its" +:+
  (phrase environment) `sC` S "and describes the" +:+ (plural userCharacteristic) +:+ 
  S "and the" +:+. (plural systemConstraint)

-- wrapper for specSysDesIntro
specSysDesF :: Bool -> Sentence -> [Section] -> Section
specSysDesF = \l_eND k_word subSec -> SRS.specSysDec [specSysDesIntro l_eND k_word] subSec

-- generalized specific system description introduction: boolean identifies whether the user wants the extended
-- or shortened ending (True) -> identifies key word pertaining to topic
specSysDesIntro ::  Bool -> Sentence -> Contents
specSysDesIntro l_end word_ = Paragraph $ S "This" +:+ (phrase section_) +:+ S "first presents the" +:+
            (phrase problemDescription) :+: S ", which gives a high-level view of the" +:+
            (phrase problem) +:+ S "to be solved. This is followed by the" +:+
            (plural solutionCharSpec) `sC` S "which presents the" +:+
            (plural assumption) `sC` (plural theory) `sC` eND l_end
            where eND (True) = (plural definition) +:+ S "and finally the" +:+
                               (phrase $ inModel ^. term) +:+ S "(":+: (getAcc ode) :+:
                               S ") that models the" +:+. word_  --FIXME: We need something to handle the use of nouns as verbs
                  eND (False) =  S "and" +:+ (plural definition)
 
-- wrapper for inModelIntro
inModelF :: Section -> Section -> Section -> Section -> [Contents] -> Section
inModelF r1 r2 r3 r4 otherContents = SRS.inModel ((inModelIntro r1 r2 r3 r4):otherContents) []

-- just need to provide the four references in order to this function
inModelIntro :: Section -> Section -> Section -> Section -> Contents
inModelIntro r1 r2 r3 r4 = Paragraph $ S "This" +:+ phrase section_ +:+ S "transforms" +:+
  S "the" +:+ phrase problem +:+ S "defined in" +:+ (makeRef r1) +:+
  S "into one which is expressed in mathematical terms. It uses concrete" +:+
  plural symbol_ +:+ S "defined in" +:+ (makeRef r2) +:+
  S "to replace the abstract" +:+ plural symbol_ +:+ S "in the" +:+
  plural model +:+ S "identified in" +:+ (makeRef r3) +:+ S "and" +:+.
  (makeRef r4)
 
-- wrapper for datConPar
datConF :: Sentence -> Sentence -> Bool -> Sentence -> [Contents] -> Section
datConF = \tr mid end t otherContents ->
  SRS.datCon ((datConPar tr mid end t):otherContents) []
  
-- reference to the input/ ouput tables -> optional middle sentence(s) (use EmptyS if not wanted) -> 
-- True if standard ending sentence wanted -> optional trailing sentence(s) -> Contents
datConPar :: Sentence -> Sentence -> Bool -> Sentence -> Contents
datConPar tableRef middleSent endingSent trailingSent = ( Paragraph $
  tableRef +:+ S "the" +:+
  plural datumConstraint +:+ S "on the" +:+ phrase input_ +:+
  S "and" +:+ phrase output_ +:+ plural variable `sC` S "respectively." +:+
  S "The" +:+ phrase column +:+ S "for" +:+ phrase physical +:+
  plural constraint +:+ S "gives the" +:+ phrase physical +:+
  plural limitation +:+ S "on the range of" +:+ plural value +:+
  S "that can be taken by the" +:+. phrase variable +:+ middleSent +:+ -- << if you are wondering where middleSent is
  S "The" +:+ plural constraint +:+ S "are conservative," +:+ S "to give the" +:+
  phrase user +:+ S "of the" +:+ phrase model +:+ S "the flexibility to" +:+ 
  S "experiment with unusual situations. The" +:+ phrase column +:+ S "of" +:+ 
  S "typical" +:+ plural value +:+ S "is intended to provide a feel for a common scenario." +:+
  endS endingSent +:+ trailingSent )
  where endS False = EmptyS
        endS True  = S "The" +:+ phrase uncertainty +:+ phrase column +:+ S "provides an" +:+
                     S "estimate of the confidence with which the" +:+ phrase physical +:+
                     plural quantity +:+. S "can be measured" +:+ S "This" +:+
                     phrase information +:+ S "would be part of the" +:+ phrase input_ +:+
                     S "if one were performing an" +:+ phrase uncertainty +:+
                     S "quantification exercise."
                     
