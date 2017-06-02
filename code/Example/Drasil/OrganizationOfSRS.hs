module Drasil.OrganizationOfSRS 
  ( refineChain
  , showingCxnBw
  , figureLabel
-- start of functions for SRS document sections in order of document apperence
  , introductionF, introF
  , prpsOfDocF
  , scpOfReqF
  , charIntRdrF
  , orgSec, orgSecWTS
  , stakehldrGeneral, stakeholderIntro
  , tClientF
  , tCustomerF
  , genSysF
  , systCon
  , specSysDesF
  , probDescF
  , termDefnF
  , physSystDesc
  , goalStmtF
  , solChSpecF
  , assumpF, assumpF'
  , thModF
  , genDefnF
  , dataDefnF
  , inModelF, inModelF'
  , datConF
  , reqF
  , nonFuncReqF
  ) where

import Language.Drasil
import Control.Lens ((^.))
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Math (equation)
import Data.Drasil.Concepts.Computation (algorithm)
import Data.Drasil.Concepts.Software (program)
import Data.Drasil.Utils (foldle, foldlsC, foldlSent, foldlList, ofThe, ofThe')
import qualified Drasil.SRS as SRS

introductionF :: CINP -> (Sentence, Sentence) -> Sentence -> (Sentence, Sentence) -> (Sentence, Sentence, Sentence) -> Bool -> (Sentence, CINP, Section, Sentence) -> Section
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
      where end = foldlSent [S "The following", phrase section_,
                  S "provides an overview of the", introduceAbb srs,
                  S "for" +:+. kSent, S "This", phrase section_, S "explains the", phrase purpose,
                  S "of this", phrase document `sC` foldlList (map ((\(x,y) -> x `ofThe` y)) (temp))]

--list is used by introF (current args passed in are the same for every example)
temp :: [(Sentence, Sentence)]
temp = [(phrase scope, phrase system), (phrase organization, phrase document), (plural characteristic, phrase intReader)]

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
showingCxnBw traceyVar contents = titleize traceyVar +:+ S "Showing the" +:+
  titleize' connection +:+ S "Between" +:+ contents

-- Complete the sentences, no need to add a period at the end of your input sentences
scpOfReqF :: Sentence -> CINP -> Sentence -> Section
scpOfReqF includes progName ending = SRS.scpOfReq [Paragraph intro] []
  where intro = foldlSent [(phrase scope) `ofThe'` (plural requirement),
                S "includes" +:+. includes, S "Given appropriate inputs, the code for",
                short progName, S "is intended to" +:+ ending]

--Characteristics of Intended Reader section
charIntRdrF :: Sentence -> Sentence -> CINP -> Sentence -> Section -> Section
charIntRdrF know und progName appStandd r = 
  SRS.charOfIR (intReaderIntro know und progName appStandd r) []

--paragraph called by charIntRdrF
intReaderIntro :: Sentence -> Sentence -> CINP -> Sentence -> Section -> [Contents]
intReaderIntro know und progName appStandd r = [Paragraph $ foldlSent [S "Reviewers of this",
  (phrase documentation), S "should have a strong knowledge in" +:+. know,
  S "The reviewers should also have an understanding of" +:+. und :+:
  appStandd, S "The", (plural user), S "of", (short progName),
  S "can have a lower level of expertise, as explained in", (makeRef r)]]

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
genSysF :: [Section] -> Contents -> [Contents] -> [Section] -> Section
genSysF sCntxt userIntro constraints systSubSec = SRS.genSysDes [genSysIntro]
  (sCntxt ++ [SRS.userChar [userIntro] [], systCon constraints systSubSec])

--generalized general system description introduction
genSysIntro :: Contents
genSysIntro = Paragraph $ foldlSent
              [S "This", phrase section_, S "provides general",
              phrase information, S "about the", phrase system `sC` S "identifies",
              S "the interfaces between the", phrase system, S "and its", phrase environment `sC`
              S "and describes the", plural userCharacteristic, S "and the", plural systemConstraint]

-- System Constraints
-- generalized if no constraints, but if there are, they can be passed through
systCon :: [Contents] -> [Section] -> Section
systCon [] subSec  = SRS.sysCon [systCon_none] subSec
            where systCon_none = Paragraph (S "There are no" +:+. plural systemConstraint)
systCon a subSec = SRS.sysCon a subSec  

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

-- give starting sentence(s), the program name, and finish the last sentence
probDescF :: Sentence -> CINP -> Sentence -> [Section] -> Section
probDescF start progName ending subSec = SRS.probDesc [Paragraph intro] subSec
  where intro = foldlSent [start, (short progName), S "is a computer", 
                (phrase $ program ^. term), S "developed to", ending]
                  
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

--List all the given inputs. Might be possible to use ofThe combinator from utils.hs
goalStmtF :: [Sentence] -> [Contents] -> Section
goalStmtF givenInputs otherContents = SRS.goalStmt ((Paragraph intro):otherContents) []
  where intro = S "Given" +:+ foldlList givenInputs `sC` S "the" +:+ plural goalStmt +: S "are"

-- kWord (ex ssp, progName), the two sections, gendef is True if you want general definitions sections,
--  ddEndSent is the ending sentence for Data Definitions, this is a 4-tuple of inputs for Data Constraints,
--  the last input is a tupple of lists of Sections for each Subsection in order.
solChSpecF :: CINP -> (Section, Section) -> Bool -> Sentence -> (Sentence, Sentence, Bool, Sentence) -> ([Contents], [Contents], [Contents], [Contents], [Contents], [Contents]) -> [Section] -> Section
solChSpecF kWord (probDes, likeChg) gendef ddEndSent (tbRef, mid, end, trail) (a,t,g,dd,i,dc) adSubSec = SRS.solCharSpec [Paragraph intro] (subSec gendef)
  where intro = foldlSent
                [S "The", plural inModel, S "that govern",
                short kWord, S "are presented in" +:+. makeRef (instModels gendef),
                S "The", phrase information, S "to understand",
                (S "meaning" `ofThe` plural inModel),
                S "and their derivation is also presented, so that the",
                plural inModel, S "can be verified"]
        subSec True  = [assumption_ True, theModels, generDefn, 
                        dataDefin, instModels True, dataConstr] ++ adSubSec
        subSec False = [assumption_ False, theModels,
                        dataDefin, instModels False, dataConstr] ++ adSubSec
        assumption_ True  = assumpF  theModels generDefn dataDefin (instModels True ) likeChg a
        assumption_ False = assumpF' theModels           dataDefin (instModels False) likeChg a
        theModels  = thModF kWord t
        generDefn  = genDefnF g
        dataDefin  = dataDefnF ddEndSent dd
        instModels True  = inModelF  probDes dataDefin theModels generDefn i
        instModels False = inModelF' probDes dataDefin theModels           i
        dataConstr = datConF tbRef mid end trail dc

 
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
          where refs (chunk, ref) = (titleize' chunk) +:+ sSqBr (makeRef ref) 
                itemsAndRefs Nothing = [(thModel, r1), (dataDefn, r3), (inModel, r4)]
                itemsAndRefs (Just genDef) = [(thModel, r1), (genDefn, genDef), (dataDefn, r3), 
                                              (inModel, r4)]

--wrapper for thModelIntro
thModF :: CINP -> [Contents] -> Section
thModF kword otherContents = SRS.thModel ((thModIntro kword):otherContents) []

-- generalized theoretical model introduction: identifies key word pertaining to topic
thModIntro :: CINP -> Contents
thModIntro k_word = Paragraph $ foldlSent
          [S "This", phrase section_, S "focuses on",
          S "the", phrase general, (plural $ equation ^. term), S "and",
          S "laws that", short k_word, S "is based on"]

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
                               S "needed to build the", plural inModel] +:+ ending

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
          S "The", plural constraint, S "are conservative, to give",
          (phrase user `ofThe` phrase model), S "the flexibility to", 
          S "experiment with unusual situations. The", phrase column, S "of", 
          S "typical", plural value, S "is intended to provide a feel for a common scenario"]
          +:+ endS endingSent +:+ trailingSent
          where endS False = EmptyS
                endS True  = S "The" +:+ phrase uncertainty +:+ phrase column +:+ S "provides an" +:+
                             S "estimate of the confidence with which the" +:+ phrase physical +:+
                             plural quantity +:+. S "can be measured" +:+ S "This" +:+
                             phrase information +:+ S "would be part of the" +:+ phrase input_ +:+
                             S "if one were performing an" +:+ phrase uncertainty +:+.
                             S "quantification exercise"

stakehldrGeneral :: CINP -> Sentence -> Section
stakehldrGeneral kWord clientDetails = (SRS.stakeholder) [stakeholderIntro] subs
  where subs = [(tClientF kWord clientDetails), (tCustomerF kWord)]

-- general stakeholders introduction
stakeholderIntro :: Contents
stakeholderIntro = Paragraph $ foldlSent [S "This", phrase section_,
            S "describes the" +: titleize' stakeholder, S "the people who have an",
            phrase interest, S "in", (phrase $ the product_)]

tClientF :: CINP -> Sentence ->  Section
tClientF kWord details = SRS.theClient [clientIntro kWord details] []

clientIntro :: CINP -> Sentence -> Contents
clientIntro kWord  details = Paragraph $ foldlSent [(at_start $ the client), S "for",
  (short kWord), S "is a", phrase company, S "named" +:+. details,
  (at_start $ the client), S "has the final say on acceptance of the", 
  phrase product_]

tCustomerF :: CINP -> Section
tCustomerF kWord = SRS.theCustomer [customerIntro kWord] []

customerIntro :: CINP -> Contents
customerIntro kWord = Paragraph $ foldlSent [(at_start' $ the customer), 
  S "are the", phrase endUser, S "of", (short kWord)]

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


-- wrapper for nonfuncReq
nonFuncReqF :: (Concept c) => [c] -> [c] -> Sentence -> Sentence -> Section
nonFuncReqF noPriority priority_ reason_ explanation_ = SRS.nonfuncReq
  [nonFuncReq (map (\x -> phrase $ x ^. term) noPriority) (map (\x -> phrase $ x ^. term) priority_) reason_ explanation_] []
        
-- generalized non-functional requirements paragraph: list of non-priority requirements, list of priority requirements,
-- reason for initial priority choice, explanation for how priority choice can be achieved.
nonFuncReq :: [Sentence] -> [Sentence] -> Sentence -> Sentence -> Contents
nonFuncReq noPriority priority_ reason_ explanation_ = Paragraph $ reason_ `sC` (listO explanation_ noPriority priority_)
listO :: Sentence -> [Sentence] -> [Sentence] -> Sentence
listO explanation_ [] [] = S "so there are no" +:+ plural priority +:+ explanation_
listO explanation_ [] priority_ = S "so" +:+ head priority_ +:+ S "is a high" +:+. phrase priority +:+ explanation_ +:+ S "The other" +:+. listT (tail priority_)
listO explanation_ [s] priority_ = S "so" +:+ s +:+ S "is not a" +:+. phrase priority +:+ explanation_ +:+ S "Rather than" +:+ s `sC` S "the" +:+. listT priority_
listO explanation_ s priority_ = S "so" +:+ foldlList s +:+ S "are not" +:+. plural priority +:+ explanation_ +:+ S "Rather, the" +:+. listT priority_
listT :: [Sentence] -> Sentence
listT [] = (phrase $ program ^. term) +:+ S "does not possess a" +:+ phrase priority +:+ phrase nonfunctionalRequirement
listT [s] = phrase nonfunctionalRequirement +:+ phrase priority +:+ S "is" +:+ s
listT s = phrase nonfunctionalRequirement +:+ plural priority +:+ S "are" +:+ foldlList s