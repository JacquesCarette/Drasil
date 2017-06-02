module Drasil.OrganizationOfSRS 
  ( showingCxnBw
  , figureLabel
-- start of functions for SRS document sections in order of document apperence
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
  ) where

import Language.Drasil
import Control.Lens ((^.))
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Math (equation)
import Data.Drasil.Concepts.Software (program)
import Data.Drasil.Utils (foldle)
import Data.Drasil.SentenceStructures
import qualified Drasil.SRS as SRS

figureLabel :: [Char] -> NPNC -> Sentence -> [Char]-> Contents
figureLabel num traceyMG contents filePath = Figure (titleize figure +: S num
  +:+ (showingCxnBw (traceyMG) (contents))) filePath

showingCxnBw :: NPNC -> Sentence -> Sentence
showingCxnBw traceyVar contents = titleize traceyVar +:+ S "Showing the" +:+
  titleize' connection +:+ S "Between" +:+ contents

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