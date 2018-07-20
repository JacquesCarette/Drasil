module Drasil.Sections.SpecificSystemDescription 
  ( specSysDescr
  , probDescF
  , termDefnF
  , physSystDesc
  , goalStmtF
  , solutionCharSpecIntro 
  , assumpF
  , thModF
  , genDefnF
  , dataDefnF
  , inModelF
  , datConF
  , dataConstraintUncertainty
  , inDataConstTbl, outDataConstTbl 
  , listofTablesToRefs
  ) where

import Language.Drasil
import Data.Drasil.Concepts.Documentation (physical, column, input_, uncertainty, physicalConstraint,
  softwareConstraint, typUnc, user, model, value, quantity, information, constraint, variable,
  output_, symbol_, limitation, problem, inModel, datum, datumConstraint, section_, dataDefn,
  general, genDefn, assumption, thModel, physicalSystem,
  likelyChg, unlikelyChg, goalStmt, purpose, requirement, element)
import Data.Drasil.Concepts.Math (equation)
import Data.Drasil.Concepts.Software (program)
import Data.Drasil.Utils (foldle, fmtU, getRVal)
import Data.Drasil.SentenceStructures (fmtPhys, fmtSfwr, mkTableFromColumns, foldlSent, 
  foldlSP, typUncr, ofThe, foldlList)
import qualified Drasil.DocLang.SRS as SRS


-- | Specific System description section builder. Takes the system and subsections.
specSysDescr :: (NamedIdea a) => a -> [Section] -> Section
specSysDescr sys subs = SRS.specSysDes [intro_ sys] subs

-- FIXME: this all should be broken down and mostly generated.
-- Generates an introduction based on the system.
intro_ :: (NamedIdea a) => a -> Contents
intro_ sys = Paragraph $ S "This section first presents the problem" +:+
  S "description, which gives a high-level view of the problem to be" +:+
  S "solved. This is followed by the solution characteristics" +:+
  S "specification, which presents the assumptions" `sC`
  S "theories, and definitions that are used for the" +:+. (phrase sys)

--Up to change, decide on what ending sentence structure we would like to employ
--Using Verbatim for now.
{-            where eND (True) = plural definition +:+ S "and finally the" +:+
                               (phrase $ inModel ^. term) +:+ sParen (getAcc ode)
                               S "that models the" +:+. word_  --FIXME: We need something to handle the use of nouns as verbs
                  eND (False) =  S "and" +:+. plural definition-}

-- give starting sentence(s), the program name, and finish the last sentence
probDescF :: (Idea a) => Sentence -> a -> Sentence -> [Section] -> Section
probDescF start progName ending subSec = SRS.probDesc [Paragraph intro] subSec
  where intro = foldlSent [start, (short progName), S "is a computer", 
                (phrase program), S "developed to", ending]
                  
--can take a (Just sentence) if needed or Nothing if not
termDefnF :: Maybe Sentence -> [Contents] -> Section
termDefnF end otherContents = SRS.termAndDefn ((intro):otherContents) []
      where lastF Nothing  = EmptyS
            lastF (Just s) = S "." +:+ s
            intro = foldlSP [S "This subsection provides a list of terms", 
                    S "that are used in the subsequent", plural section_, 
                    S "and their meaning, with the", phrase purpose, 
                    S "of reducing ambiguity and making it easier to correctly", 
                    S "understand the", plural requirement :+: (lastF end)]

--general introduction for Physical System Description
physSystDesc :: Sentence -> LabelledContent -> [LabelledContent] -> Section
physSystDesc progName fg otherContents = SRS.physSyst ((intro):otherContents) []
  where intro = llcc "physSystDescIntro" (mkLabelRA'' "physSystDescSection") $ 
                Paragraph $ foldle (+:+) (+:) (EmptyS)
                [S "The", (phrase physicalSystem), S "of", progName `sC`
                S "as shown in", (makeRef fg) `sC` S "includes the following", 
                plural element]

--List all the given inputs. Might be possible to use ofThe combinator from utils.hs
goalStmtF :: [Sentence] -> [Contents] -> Section
goalStmtF givenInputs otherContents = SRS.goalStmt (intro:otherContents) []
  where intro = Paragraph $ S "Given" +:+ foldlList givenInputs `sC` S "the" +:+ 
                plural goalStmt +: S "are"


solutionCharSpecIntro :: (Idea a) => a -> Section -> Contents
solutionCharSpecIntro progName instModelSection = foldlSP [S "The", plural inModel, 
  S "that govern", short progName, S "are presented in" +:+. 
  makeRef (instModelSection), S "The", phrase information, S "to understand", 
  (S "meaning" `ofThe` plural inModel), 
  S "and their derivation is also presented, so that the", plural inModel, 
  S "can be verified"]


-- wrappers for assumpIntro. Use assumpF' if genDefs is not needed
assumpF :: (HasShortName a, Referable a, HasShortName b, Referable b, HasShortName c, Referable c) => 
  Label -> Label -> Label -> b -> c -> a -> [LabelledContent] -> Section
assumpF theMod genDef dataDef inMod likeChg unlikeChg otherContents = 
      SRS.assumpt ((assumpIntro theMod genDef dataDef inMod likeChg unlikeChg):otherContents) []


-- takes a bunch of references to things discribed in the wrapper
assumpIntro :: (HasShortName a, Referable a, HasShortName b, Referable b, HasShortName c, Referable c) =>
  Label -> Label -> Label -> b -> c -> a -> LabelledContent
assumpIntro r1 r2 r3 r4 r5 r6 = llcc "assumpIntroLC" (mkLabelRA'' "assumpIntro") $ Paragraph $ foldlSent 
          [S "This", (phrase section_), S "simplifies the original", (phrase problem), 
          S "and helps in developing the", (phrase thModel), S "by filling in the", 
          S "missing", (phrase information), S "for the" +:+. (phrase physicalSystem), 
          S "The numbers given in the square brackets refer to the", 
          foldr1 sC (map refs itemsAndRefs) `sC` (refs (inModel, r4)) `sC` (refs (likelyChg, r5)) `sC`
          S "or", refs (unlikelyChg, r6) `sC` S "in which the respective", 
          (phrase assumption), S "is used"] --FIXME: use some clever "zipWith"
          where refs (chunk, ref) = (titleize' chunk) +:+ sSqBr (makeRef ref) 
                itemsAndRefs = [(thModel, r1), (genDefn, r2), (dataDefn, r3)]

--wrapper for thModelIntro
thModF :: (Idea a) => a -> [Contents] -> Section
thModF progName otherContents = SRS.thModel ((thModIntro progName):otherContents) []

-- generalized theoretical model introduction: identifies key word pertaining to topic
thModIntro :: (Idea a) => a -> Contents
thModIntro progName = foldlSP
          [S "This", phrase section_, S "focuses on", 
          S "the", phrase general, (plural equation), S "and", 
          S "laws that", short progName, S "is based on"]

-- just supply the other contents for General Definition. Use empty list if none needed
genDefnF :: [LabelledContent] -> Section
genDefnF otherContents = SRS.genDefn ((llcc "gdIntroLC" (mkLabelRA'' "gdIntroLC") $ 
  generalDefinitionIntro otherContents):otherContents) []

generalDefinitionIntro :: (Referable t) => [t] -> Contents
generalDefinitionIntro [] = Paragraph $ S "There are no general definitions."
generalDefinitionIntro _ = foldlSP [S "This", phrase section_, 
  S "collects the", S "laws and", (plural equation), 
  S "that will be used in", S "deriving the", 
  plural dataDefn `sC` S "which in turn are used to", S "build the", 
  plural inModel]

-- uses EmptyS if ending sentence is not needed
dataDefnF :: Sentence -> [LabelledContent] -> Section                      
dataDefnF endingSent otherContents = SRS.dataDefn 
  ((dataDefinitionIntro endingSent):otherContents) []

dataDefinitionIntro :: Sentence -> LabelledContent
dataDefinitionIntro closingSent = llcc "ddIntroLC" (mkLabelRA'' "ddIntro") $ 
  Paragraph $ (foldlSent [S "This", phrase section_, 
    S "collects and defines all the", plural datum, 
    S "needed to build the", plural inModel] +:+ closingSent)

-- wrappers for inModelIntro. Use inModelF' if genDef are not needed
inModelF :: Section -> Section -> Section -> Label -> [Contents] -> Section
inModelF probDes datDef theMod genDef otherContents = SRS.inModel ((inModelIntro probDes datDef theMod genDef):otherContents) []

-- just need to provide the four references in order to this function. Nothing can be input into r4 if only three tables are present
inModelIntro :: Section -> Section -> Section -> Label -> Contents
inModelIntro r1 r2 r3 r4 = foldlSP [S "This", phrase section_, 
  S "transforms the", phrase problem, S "defined in", (makeRef r1), 
  S "into one which is expressed in mathematical terms. It uses concrete", 
  plural symbol_, S "defined in", (makeRef r2), 
  S "to replace the abstract", plural symbol_, S "in the", 
  plural model, S "identified in", (makeRef r3) :+: end]
    where end = S " and" +:+ (makeRef r4)

-- wrapper for datConPar
datConF :: Sentence -> Sentence -> Sentence -> [LabelledContent] -> Section
datConF hasUncertainty mid trailing tables = SRS.datCon 
  ((llcc "dataConstraintsIntro" (mkLabelRA'' "dataConsSection") $
    dataConstraintParagraph hasUncertainty (listofTablesToRefs tables) mid trailing):
    tables) []
  
-- reference to the input/ ouput tables -> optional middle sentence(s) (use EmptyS if not wanted) -> 
-- True if standard ending sentence wanted -> optional trailing sentence(s) -> Contents
dataConstraintParagraph :: Sentence -> Sentence -> Sentence -> Sentence -> Contents
dataConstraintParagraph hasUncertainty tableRef middleSent trailingSent = Paragraph $
  (dataConstraintIntroSent tableRef) +:+ middleSent +:+ 
  (dataConstraintClosingSent hasUncertainty trailingSent)

-- makes a list of references to tables takes
-- l  list of layout objects that can be referenced
-- outputs a sentence containing references to the layout objects 
listofTablesToRefs :: (HasShortName l, Referable l) => [l] -> Sentence
listofTablesToRefs  []     = EmptyS
listofTablesToRefs  [x]    = (makeRef x) +:+ S "shows"
listofTablesToRefs  [x,y]  = (makeRef x) +:+ S "and" +:+ (makeRef y) +:+ S "show" -- for proper grammar with multiple tables
                                                                                  -- no Oxford comma in case there is only two tables to be referenced
listofTablesToRefs  (x:xs) = (makeRef x) `sC` listofTablesToRefs (xs)
 
dataConstraintIntroSent :: Sentence -> Sentence
dataConstraintIntroSent tableRef = foldlSent [tableRef, S "the", plural datumConstraint, S "on the", phrase input_, 
  S "and", phrase output_ +:+. (plural variable `sC` S "respectively"), S "The", 
  phrase column, S "for", phrase physical, plural constraint, S "gives the", 
  phrase physical, plural limitation, S "on the range of", plural value, 
  S "that can be taken by the", phrase variable]

dataConstraintClosingSent :: Sentence -> Sentence -> Sentence
dataConstraintClosingSent uncertaintySent trailingSent = foldlSent
  [S "The", plural constraint, S "are conservative, to give", 
  (phrase user `ofThe` phrase model), S "the flexibility to", 
  S "experiment with unusual situations. The", phrase column, S "of", S "typical",
  plural value, S "is intended to provide a feel for a common scenario"]
  +:+ uncertaintySent +:+ trailingSent

dataConstraintUncertainty :: Sentence
dataConstraintUncertainty = foldlSent [S "The", phrase uncertainty, phrase column,
  S "provides an estimate of the confidence with which the", phrase physical,
  plural quantity +:+. S "can be measured", S "This", phrase information,
  S "would be part of the", phrase input_, S "if one were performing an",
  phrase uncertainty, S "quantification exercise"]

-- Creates the input Data Constraints Table
inDataConstTbl :: (UncertainQuantity c, Constrained c, HasReasVal c) => [c] -> LabelledContent
inDataConstTbl qlst = llcc "InDataConstraints" (mkLabelRA'' "InDataConstraints") $
  Table titl cts (S "Input Data Constraints") True "InDataConstraints"
  where
   columns = [(S "Var", map ch qlst),
            (titleize' physicalConstraint, map fmtPhys qlst),
            (titleize' softwareConstraint, map fmtSfwr qlst),
            (S "Typical Value", map (\q -> fmtU (E $ getRVal q) q) qlst),
            (short typUnc, map typUncr qlst)]
   tbl = mkTableFromColumns columns
   titl = fst tbl
   cts = snd tbl

-- Creates the output Data Constraints Table
outDataConstTbl :: (Quantity c, Constrained c) => [c] -> LabelledContent
outDataConstTbl qlst = llcc "OutDataConstraints" (mkLabelRA'' "OutDataConstraints") $
  Table titl cts (S "Output Data Constraints") True "OutDataConstraints"
  where
   columns = [(S "Var", map ch qlst),
            (titleize' physicalConstraint, map fmtPhys qlst),
            (titleize' softwareConstraint, map fmtSfwr qlst)]
   tbl = mkTableFromColumns columns
   titl = fst tbl
   cts = snd tbl
