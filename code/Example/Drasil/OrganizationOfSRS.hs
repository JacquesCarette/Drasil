module Drasil.OrganizationOfSRS (refineChain, orgSec, orgSecWTS, genSysF, 
                                 specSysDesF, datConF, datConPar, reqF,
                                 figureLabel, showingCxnBw, thModF, inModelF,
                                 inModelF', traceMGF, systCon) where

import Language.Drasil
import Control.Lens ((^.))
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Math(equation)
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
orgSec i b s = Section (titleize orgOfDoc) (map Con (orgIntro i b s Nothing))

-- | Same as 'orgSec' with the addition of extra information at the end 
-- (post-refine chain)?
orgSecWTS :: (NounPhrase c) => Sentence -> c -> Section -> Sentence -> Section
orgSecWTS i b s t = Section (titleize orgOfDoc) (map Con (orgIntro i b s (Just t)))
  
  
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
genSysF = SRS.genSysDes [genSysIntro]

--generalized general system description introduction
genSysIntro :: Contents
genSysIntro = Paragraph $ S "This" +:+ phrase section_ +:+ S "provides general" +:+
  phrase information +:+ S "about the" +:+ phrase system `sC` S "identifies" +:+
  S "the interfaces between the" +:+ phrase system +:+ S "and its" +:+
  phrase environment `sC` S "and describes the" +:+ plural userCharacteristic +:+ 
  S "and the" +:+. plural systemConstraint

-- wrapper for specSysDesIntro
specSysDesF :: Sentence -> [Section] -> Section
specSysDesF l_eND subSec = SRS.specSysDes [specSysDesIntro l_eND] subSec

-- generalized specific system description introduction: boolean identifies whether the user wants the extended
-- or shortened ending (True) -> identifies key word pertaining to topic or Nothing
specSysDesIntro ::  Sentence -> Contents
specSysDesIntro l_end = Paragraph $ S "This" +:+ phrase section_ +:+ S "first presents the" +:+
            phrase problemDescription `sC` S "which gives a high-level view of the" +:+
            phrase problem +:+ S "to be solved. This is followed by the" +:+
            plural solutionCharSpec `sC` S "which presents the" +:+
            plural assumption `sC` plural theory `sC` l_end

--Up to change, decide on what ending sentence structure we would like to employ
--Using Verbatim for now.
{-            where eND (True) = plural definition +:+ S "and finally the" +:+
                               (phrase $ inModel ^. term) +:+ sParen (getAcc ode)
                               S "that models the" +:+. word_  --FIXME: We need something to handle the use of nouns as verbs
                  eND (False) =  S "and" +:+. plural definition-}

--wrapper for thModelIntro
thModF :: Sentence -> [Contents] -> Section
thModF kword otherContents = SRS.thModel ((thModIntro kword):otherContents) []

-- generalized theoretical model introduction: identifies key word pertaining to topic
thModIntro :: Sentence -> Contents
thModIntro k_word = Paragraph $ S "This" +:+ phrase section_ +:+ S "focuses on" +:+
  S "the" +:+ phrase general +:+ (plural $ equation ^. term) +:+ S "and" +:+
  S "laws that" +:+ (k_word) +:+. S "is based on"

-- wrappers for inModelIntro. Use inModelF' if genDef are not needed
inModelF :: Section -> Section -> Section -> Section -> [Contents] -> Section
inModelF probDes datDef theMod genDef otherContents = SRS.inModel ((inModelIntro probDes datDef theMod (Just genDef)):otherContents) []

inModelF' :: Section -> Section -> Section -> [Contents] -> Section
inModelF' probDes datDef theMod otherContents = SRS.inModel ((inModelIntro probDes datDef theMod Nothing):otherContents) []

-- just need to provide the four references in order to this function. Nothing can be input into r4 if only three tables are present
inModelIntro :: Section -> Section -> Section -> Maybe Section -> Contents
inModelIntro r1 r2 r3 r4 = Paragraph $ S "This" +:+ phrase section_ +:+ S "transforms" +:+
  S "the" +:+ phrase problem +:+ S "defined in" +:+ (makeRef r1) +:+
  S "into one which is expressed in mathematical terms. It uses concrete" +:+
  plural symbol_ +:+ S "defined in" +:+ (makeRef r2) +:+
  S "to replace the abstract" +:+ plural symbol_ +:+ S "in the" +:+
  plural model +:+ S "identified in" +:+ (makeRef r3) :+: end r4
  where end (Just genDef) = S " and" +:+. (makeRef genDef)
        end Nothing       = S "."
        
-- wrapper for datConPar
datConF :: Sentence -> Sentence -> Bool -> Sentence -> [Contents] -> Section
datConF tr mid end t otherContents = SRS.datCon ((datConPar tr mid end t):otherContents) []
  
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
                     
-- wrapper for reqIntro
reqF :: [Section] -> Section
reqF = SRS.require [reqIntro]

--generalized requirements introduction
reqIntro :: Contents
reqIntro = Paragraph $ S "This" +:+ phrase section_ +:+ S "provides the" +:+
  plural functionalRequirement `sC` S "the business tasks that the" +:+
  phrase software +:+ S "is expected to complete, and the" +:+
  plural nonfunctionalRequirement `sC` S "the qualities that the" +:+
  phrase software +:+. S "is expected to exhibit"

-- wrapper for traceMGIntro
traceMGF :: Contents -> Contents -> Contents -> [Contents] -> [Section] -> Section
traceMGF rf1 rf2 rf3 otherContents subSec = SRS.traceyMandG ((traceMGIntro rf1 rf2 rf3):otherContents) subSec

-- generalized traceability matrix and graph introduction: variables are references to the three tables
-- generally found in this section (in order of being mentioned)
traceMGIntro :: Contents -> Contents -> Contents -> Contents
traceMGIntro r1 r2 r3 = Paragraph $ S "The" +:+ phrase purpose +:+ S "of the" +:+
  plural traceyMatrix +:+ S "is to provide easy" +:+
  plural reference +:+ S "on what has to be additionally modified if a" +:+
  S "certain" +:+ phrase component +:+ S "is changed. Every time a" +:+
  phrase component +:+ S "is changed, the" +:+ plural item +:+ S "in the" +:+
  phrase column +:+ S "of that" +:+ phrase component +:+ S "that are" +:+
  S "marked with an" +:+ Quote (S "X") +:+. S "should be modified as well" +:+
  (makeRef r1) +:+ S "shows the dependencies of" +:+ plural thModel `sC`
  plural genDefn `sC` plural dataDefn `sC`
  S "and" +:+ plural inModel +:+. S "with each other" +:+ (makeRef r2) +:+
  S "shows the dependencies of" +:+ plural inModel `sC`
  plural requirement `sC` S "and" +:+ plural datum +:+ plural constraint +:+.
  S "on each other" +:+ (makeRef r3) +:+ S "shows the dependencies of" +:+ 
  plural thModel `sC` plural genDefn `sC` plural dataDefn `sC`
  plural inModel `sC` S "and" +:+ plural likelyChg +:+ S "on the" +:+.
  titleize' assumption

-- System Constraints
-- generalized if no constraints, but if there are, they can be passed through
systCon :: Maybe Contents -> [Section] -> Section
systCon (Just a) subSec = SRS.sysCon [a] subSec
systCon Nothing subSec  = SRS.sysCon [systCon_none] subSec
  where systCon_none = Paragraph (S "There are no" +:+. plural systemConstraint)




