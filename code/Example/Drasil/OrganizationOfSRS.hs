module Drasil.OrganizationOfSRS (introF, prpsOfDocF, refineChain, orgSec, orgSecWTS, genSysF, 
                                 specSysDesF, termDefnF, solChSpecF, assumpF, assumpF', datConF, reqF,
                                 figureLabel, showingCxnBw, thModF, genDefnF, inModelF,
                                 dataDefnF, inModelF', traceMGF, systCon, stakehldr,
                                 stakeholderIntro, traceGIntro, physSystDesc) where

import Language.Drasil
import Control.Lens ((^.))
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Math (equation, matrix, graph)
import Data.Drasil.Concepts.Computation (algorithm)
import Data.Drasil.Utils (foldle, foldlsC, foldlSent)
import qualified Drasil.SRS as SRS

--Provide the start to the intro, then the key sentence relating to the overview, and subsections
introF :: Sentence -> Sentence -> [Section] -> Section
introF start kSent subSec = SRS.intro [Paragraph start, Paragraph end] subSec
      where end = foldlSent [S "The following", phrase section_,
                  S "provides an overview of the", introduceAbb srs,
                  S "for", kSent, S "This", phrase section_, S "explains the", phrase purpose,
                  S "of this", phrase document `sC` S "the", phrase scope,
                  S "of the", phrase system `sC` S "the", phrase organization,
                  S "of the", phrase document, S  "and the",
                  plural characteristic, S "of the", plural intReader]

-- provide only the first paragraph (as a sentence type) to 
prpsOfDocF :: Sentence -> Section
prpsOfDocF par1 = SRS.prpsOfDoc [Paragraph par1, Paragraph par2] []
      where par2 = foldlSent [S "This", phrase document, 
                    S "will be used as a starting point for subsequent development", 
                    S "phases, including writing the", phrase desSpec, S "and the", 
                    phrase softwareVAV, S "plan. The", phrase designDoc,
                    S "will show how the", plural requirement, S "are to be realized, including",
                    plural decision, S "on the numerical", (plural $ algorithm ^. term), 
                    S "and programming" +:+. phrase environment, S "The", phrase vavPlan, 
                    S "will show the steps that will be used to increase confidence in the",
                    phrase softwareDoc, S "and the" +:+. phrase implementation, S "Although",
                    S "the", short srs, S "fits in a series of", plural document, 
                    S "that follow the so-called waterfall", phrase model `sC` 
                    S "the actual development process is not constrained", 
                    S "in any way. Even when the waterfall model is not followed, as",
                    S "Parnas and Clements point out, the most logical way", --FIXME: add citation to these people?
                    S "to present the", phrase documentation, S "is still to",
                    Quote (S "fake"), S "a rational", phrase design, S "process"]

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
showingCxnBw traceyMG contents = foldlSent [titleize traceyMG, S "Showing the",
  titleize' connection, S "Between", contents]

-- | Organization of the document section builder. Takes an introduction,
-- a "bottom" chunk (where to start reading bottom-up. Usually instance
-- models or data definitions), a bottom section (for creating a reference link)
-- which should match the bottom chunk, but does not have to.
orgSec :: (NounPhrase c) => Sentence -> c -> Section -> Section
orgSec i b s = SRS.orgOfDoc (orgIntro i b s Nothing) []

-- | Same as 'orgSec' with the addition of extra information at the end 
-- (post-refine chain)?
orgSecWTS :: (NounPhrase c) => Sentence -> c -> Section -> Sentence -> Section
orgSecWTS i b s t = SRS.orgOfDoc (orgIntro i b s (Just t)) []

-- Intro -> Bottom (for bottom up approach) -> Section that contains bottom ->
--    trailing sentences -> [Contents]
orgIntro :: (NounPhrase c) => Sentence -> c -> Section -> Maybe Sentence -> [Contents]
orgIntro intro bottom bottomSec trailingSentence = [Paragraph $ foldlSent [
          intro, S "The presentation follows the standard pattern of presenting",
          (foldlsC $ map plural [goal, theory, definition]) `sC` S "and assumptions.",
          S "For readers that would like a more bottom up approach" `sC`
          S "they can start reading the", plural bottom, 
          S "in", makeRef bottomSec +:+
          S "and trace back to find any additional information they require"],
          Paragraph $ lastS trailingSentence]
          where lastS Nothing = refineChain [goalStmt, thModel, inModel]
                lastS (Just t) = lastS Nothing +:+. t

-- wrapper for general system description
genSysF :: [Section] -> Section
genSysF = SRS.genSysDes [genSysIntro]

--generalized general system description introduction
genSysIntro :: Contents
genSysIntro = Paragraph $ foldlSent
              [S "This", phrase section_, S "provides general",
              phrase information, S "about the", phrase system `sC` S "identifies",
              S "the interfaces between the", phrase system, S "and its", phrase environment `sC`
              S "and describes the", plural userCharacteristic, S "and the", plural systemConstraint]

-- System Constraints
-- generalized if no constraints, but if there are, they can be passed through
systCon :: Maybe [Contents] -> [Section] -> Section
systCon (Just a) subSec = SRS.sysCon a subSec
systCon Nothing subSec  = SRS.sysCon [systCon_none] subSec
            where systCon_none = Paragraph (S "There are no" +:+. plural systemConstraint)  

-- wrapper for specSysDesIntro
specSysDesF :: Sentence -> [Section] -> Section
specSysDesF l_eND subSec = SRS.specSysDes [specSysDesIntro l_eND] subSec

-- generalized specific system description introduction: boolean identifies whether the user wants the extended
-- or shortened ending (True) -> identifies key word pertaining to topic or Nothing
specSysDesIntro ::  Sentence -> Contents
specSysDesIntro l_end = Paragraph $ foldlSent
            [S "This", phrase section_, S "first presents the",
            phrase problemDescription `sC` S "which gives a high-level view of the",
            phrase problem, S "to be solved. This is followed by the",
            plural solutionCharSpec `sC` S "which presents the",
            plural assumption `sC` plural theory `sC` l_end]

--Up to change, decide on what ending sentence structure we would like to employ
--Using Verbatim for now.
{-            where eND (True) = plural definition +:+ S "and finally the" +:+
                               (phrase $ inModel ^. term) +:+ sParen (getAcc ode)
                               S "that models the" +:+. word_  --FIXME: We need something to handle the use of nouns as verbs
                  eND (False) =  S "and" +:+. plural definition-}

--can take a (Just sentence) if needed or Nothing if not
termDefnF :: Maybe Sentence -> [Contents] -> Section
termDefnF end otherContents = SRS.termAndDefn ((intro):otherContents) []
      where lastF Nothing  = EmptyS
            lastF (Just s) = s
            intro = Paragraph $ foldle (+:+) (+:) (EmptyS)
                    [S "This subsection provides a list of terms",
                    S "that are used in the subsequent", plural section_, S "and their",
                    S "meaning, with the", phrase purpose, S "of reducing ambiguity",
                    S "and making it easier to correctly understand the",
                    plural requirement, lastF end]

--general introduction for Physical System Description
physSystDesc :: Sentence -> Contents -> [Contents] -> Section
physSystDesc kWord fig otherContents = SRS.physSyst ((intro):otherContents) []
  where intro = Paragraph $ foldle (+:+) (+:) (EmptyS)
                [S "The", (phrase physicalSystem), S "of", kWord `sC`
                S "as shown in", (makeRef fig) `sC` S "includes the following", plural element]

--provide the key word, a reference to the Instance Model, and the Subsections
solChSpecF :: CINP -> Section -> [Section] -> Section
solChSpecF kWord inModRef subSec = SRS.solCharSpec [Paragraph intro] subSec
      where intro = foldlSent
                    [S "The", plural inModel, S "that govern",
                    short kWord, S "are presented in" +:+. makeRef inModRef,
                    S "The", phrase information, S "to understand the meaning of the",
                    plural inModel, S "and their derivation is also presented, so that the",
                    plural inModel, S "can be verified"]

 
-- wrappers for assumpIntro. Use assumpF' if genDefs is not needed
assumpF :: Section -> Section -> Section -> Section -> Section -> [Contents] -> Section
assumpF theMod genDef dataDef inMod likeChg otherContents = 
      SRS.assump ((assumpIntro theMod (Just genDef) dataDef inMod likeChg):otherContents) []

assumpF' :: Section -> Section -> Section -> Section -> [Contents] -> Section
assumpF' theMod dataDef inMod likeChg otherContents = 
      SRS.assump ((assumpIntro theMod Nothing dataDef inMod likeChg):otherContents) []

-- takes a bunch of references to things discribed in the wrapper
assumpIntro :: Section -> Maybe Section -> Section -> Section -> Section -> Contents
assumpIntro r1 r2 r3 r4 r5 = Paragraph $ foldlSent 
          [S "This", (phrase section_), S "simplifies the original", (phrase problem),
          S "and helps in developing the", (phrase thModel), S "by filling in the",
          S "missing", (phrase information), S "for the" +:+. (phrase physicalSystem),
          S "The numbers given in the square brackets refer to the", 
          foldr1 sC (map (refs) (itemsAndRefs r2)) `sC` S "or", 
          refs (likelyChg, r5) `sC` S "in which the respective",
          (phrase assumption), S "is used"] --FIXME: use some clever "zipWith"
          where refs (chunk, ref) = (titleize' chunk) +:+ S "[" :+: (makeRef ref) :+: S "]" 
                itemsAndRefs Nothing = [(thModel, r1), (dataDefn, r3), (inModel, r4)]
                itemsAndRefs (Just genDef) = [(thModel, r1), (genDefn, genDef), (dataDefn, r3), 
                                              (inModel, r4)]

--wrapper for thModelIntro
thModF :: Sentence -> [Contents] -> Section
thModF kword otherContents = SRS.thModel ((thModIntro kword):otherContents) []

-- generalized theoretical model introduction: identifies key word pertaining to topic
thModIntro :: Sentence -> Contents
thModIntro k_word = Paragraph $ foldlSent
          [S "This", phrase section_, S "focuses on",
          S "the", phrase general, (plural $ equation ^. term), S "and",
          S "laws that", k_word, S "is based on"]

-- just supply the other contents for General Definition. Use empty list if none needed
genDefnF :: [Contents] -> Section
genDefnF otherContents = SRS.genDefn (genDefnIntro:otherContents) []
  where genDefnIntro = Paragraph $ foldlSent [S "This", phrase section_, S "collects the",
                       S "laws and", (plural $ equation ^. term), S "that will be used in", 
                       S "deriving the", plural dataDefn `sC` S "which in turn are used to",
                       S "build the", plural inModel]
                       
-- uses EmptyS if ending sentence is not needed
dataDefnF :: Sentence -> [Contents] -> Section                      
dataDefnF endingSent otherContents = SRS.dataDefn ((dataDefnIntro endingSent):otherContents) []
  where dataDefnIntro ending = Paragraph $ foldlSent [S "This", phrase section_, 
                               S "collects and defines all the", plural datum,
                               S "needed to build the" +:+ plural inModel, ending]

-- wrappers for inModelIntro. Use inModelF' if genDef are not needed
inModelF :: Section -> Section -> Section -> Section -> [Contents] -> Section
inModelF probDes datDef theMod genDef otherContents = SRS.inModel ((inModelIntro probDes datDef theMod (Just genDef)):otherContents) []

inModelF' :: Section -> Section -> Section -> [Contents] -> Section
inModelF' probDes datDef theMod otherContents = SRS.inModel ((inModelIntro probDes datDef theMod Nothing):otherContents) []

-- just need to provide the four references in order to this function. Nothing can be input into r4 if only three tables are present
inModelIntro :: Section -> Section -> Section -> Maybe Section -> Contents
inModelIntro r1 r2 r3 r4 = Paragraph $ foldlSent [S "This", phrase section_,
          S "transforms the", phrase problem, S "defined in", (makeRef r1),
          S "into one which is expressed in mathematical terms. It uses concrete",
          plural symbol_, S "defined in", (makeRef r2),
          S "to replace the abstract", plural symbol_, S "in the",
          plural model, S "identified in", (makeRef r3) :+: end r4]
          where end (Just genDef) = S " and" +:+ (makeRef genDef)
                end Nothing       = EmptyS
        
-- wrapper for datConPar
datConF :: Sentence -> Sentence -> Bool -> Sentence -> [Contents] -> Section
datConF tr mid end t otherContents = SRS.datCon ((datConPar tr mid end t):otherContents) []
  
-- reference to the input/ ouput tables -> optional middle sentence(s) (use EmptyS if not wanted) -> 
-- True if standard ending sentence wanted -> optional trailing sentence(s) -> Contents
datConPar :: Sentence -> Sentence -> Bool -> Sentence -> Contents
datConPar tableRef middleSent endingSent trailingSent = Paragraph $ foldlSent [
          tableRef, S "the", plural datumConstraint, S "on the", phrase input_,
          S "and", phrase output_, plural variable `sC` S "respectively.",
          S "The", phrase column, S "for", phrase physical,
          plural constraint, S "gives the", phrase physical,
          plural limitation, S "on the range of", plural value,
          S "that can be taken by the" +:+. phrase variable, middleSent, -- << if you are wondering where middleSent is
          S "The", plural constraint, S "are conservative, to give the",
          phrase user, S "of the", phrase model, S "the flexibility to", 
          S "experiment with unusual situations. The", phrase column, S "of", 
          S "typical", plural value, S "is intended to provide a feel for a common scenario"]
          +:+ endS endingSent +:+ trailingSent
          where endS False = EmptyS
                endS True  = S "The " +:+ phrase uncertainty +:+ phrase column +:+ S "provides an" +:+
                             S "estimate of the confidence with which the" +:+ phrase physical +:+
                             plural quantity +:+. S "can be measured" +:+ S "This" +:+
                             phrase information +:+ S "would be part of the" +:+ phrase input_ +:+
                             S "if one were performing an" +:+ phrase uncertainty +:+.
                             S "quantification exercise"
       
-- wrapper for stakeholderIntro
stakehldr :: [Section] -> Section
stakehldr subs = (SRS.stakeholder) [stakeholderIntro] subs

-- general stakeholders introduction
stakeholderIntro :: Contents
stakeholderIntro = Paragraph $ foldlSent [S "This", phrase section_,
            S "describes the" +: titleize' stakeholder, S "the people who have an",
            phrase interest, S "in", (phrase $ the product_)]

-- wrapper for reqIntro
reqF :: [Section] -> Section
reqF = SRS.require [reqIntro]

--generalized requirements introduction
reqIntro :: Contents
reqIntro = Paragraph $ foldlSent
        [S "This", phrase section_, S "provides the",
        plural functionalRequirement `sC` S "the business tasks that the",
        phrase software, S "is expected to complete, and the", 
        plural nonfunctionalRequirement `sC` S "the qualities that the",
        phrase software, S "is expected to exhibit"]

-- wrapper for traceMGIntro
traceMGF :: [Contents] -> [Sentence] -> [Contents] -> [Section] -> Section
traceMGF refs trailing otherContents subSec = SRS.traceyMandG ((traceMIntro refs trailing):otherContents) subSec

-- generalized traceability matrix and graph introduction: variables are references to the three tables
-- generally found in this section (in order of being mentioned)
traceMIntro :: [Contents] -> [Sentence] -> Contents
traceMIntro refs trailings = Paragraph $ foldlSent [S "The", phrase purpose, S "of the",
        plural traceyMatrix, S "is to provide easy", plural reference,
        S "on what has to be additionally modified if a certain", phrase component,
        S "is changed. Every time a", phrase component, S "is changed, the", plural item, S "in the",
        phrase column, S "of that", phrase component, S "that are",
        S "marked with an", Quote (S "X") +:+. S "should be modified as well",
        foldlSent (zipWith tableShows refs trailings)]

tableShows :: Contents -> Sentence -> Sentence
tableShows ref trailing = (makeRef ref) +:+ S "shows the" +:+
        plural dependency +:+ S "of" +:+ trailing

-- generalized traceability matrix and graph introduction: variables are references to the three tables
-- generally found in this section (in order of being mentioned)
traceGIntro :: [Contents] -> [Sentence] -> [Contents]
traceGIntro refs trailings = [Paragraph $ foldlSent
        [S "The", phrase purpose, S "of the", plural traceyGraph,
        S "is also to provide easy", plural reference, S "on what has to be",
        S "additionally modified if a certain", phrase component +:+. S "is changed", 
        S "The arrows in the", (plural $ graph ^. term), S "represent" +:+.
        plural dependency, S "The", phrase component, S "at the tail of an arrow",
        S "is depended on by the", phrase component, S "at the head of that arrow. Therefore, if a",
        phrase component, S "is changed, the", plural component, S "that it points to should also" +:+.
        S "be changed", foldlSent (zipWith tableShows refs trailings)],
        Paragraph $ foldlSent [S "NOTE: Building a tool to automatically generate the graphical",
        S "representation of the", (phrase $ matrix ^. term), S "by scanning the",
        plural label, S "and", phrase reference, S "can be future work"]]