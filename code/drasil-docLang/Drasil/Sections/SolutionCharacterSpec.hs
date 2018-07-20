{-# Language GADTs #-}
module Drasil.Sections.SolutionCharacterSpec
  (
  SecItem,
  SubSec,
  sSubSec,
  assembler,
  siCon,
  siSect,
  siTMod,
  siIMod,
  siDDef,
  siSent,
  siSTitl,
  siCC,
  siUQI,
  siUQO
  ) where

import Language.Drasil
import Data.Drasil.Concepts.Math (equation, law)
import Data.Drasil.Concepts.Computation (computer)
import Data.Drasil.Concepts.Software (program)
import Data.Drasil.Utils (foldle)
import Data.Drasil.SentenceStructures (ofThe, foldlSP, foldlSent, foldlList, sAnd)
import qualified Data.Drasil.Concepts.Documentation as Doc
import Data.List (find)
import Control.Lens ((^.))
import Drasil.Sections.SpecificSystemDescription (inDataConstTbl, outDataConstTbl,
  listofTablesToRefs)

import Drasil.Sections.GeneralSystDesc(genSysIntro)

import qualified Drasil.DocLang.SRS as SRS

data SecItem where 
  Cont      :: [Contents] -> SecItem
  Sect      :: [Section] -> SecItem
  TMods     :: [RelationConcept] -> SecItem
  IMods     :: [RelationConcept] -> SecItem
  DataDef   :: [QDefinition] -> SecItem
  --GenDef    :: [RelationConcept] -> SecItem
  ConChunk  :: [ConceptChunk] -> SecItem
  Sent      :: [Sentence] -> SecItem
  UnQuantI  :: [UncertQ] -> SecItem
  UnQuantO  :: [UncertQ] -> SecItem 
  SingularTitle :: SecItem


data SubSec where
  SectionModel :: NamedIdea c => c -> [SecItem] -> SubSec

sSubSec :: (NamedIdea c) => c -> [SecItem] -> SubSec
sSubSec sectionName xs = SectionModel sectionName xs

--------------------------
-- SECITEM CONSTRUCTORS --
--------------------------

siCon :: [Contents] -> SecItem
siCon xs = Cont xs

siSect :: [Section] -> SecItem
siSect xs = Sect xs

siTMod :: [RelationConcept] -> SecItem
siTMod xs = TMods xs

siIMod :: [RelationConcept] -> SecItem
siIMod xs = IMods xs

siDDef :: [QDefinition] -> SecItem
siDDef xs = DataDef xs

siSent :: [Sentence] -> SecItem
siSent xs = Sent xs

siSTitl :: SecItem
siSTitl = SingularTitle

siCC :: [ConceptChunk] -> SecItem
siCC xs = ConChunk xs

siUQI :: [UncertQ] -> SecItem
siUQI xs = UnQuantI xs

siUQO :: [UncertQ] -> SecItem
siUQO xs = UnQuantO xs
----------------------
--  HELPER FUNCTION --
----------------------

compareID :: (NamedIdea a) => a -> UID -> Bool
compareID c1 c2 = (c1 ^. uid) == c2

-----------------------
-- CHECK FOR SECITEM --
-----------------------

hasTitle :: SecItem -> Bool
hasTitle (SingularTitle) = True
hasTitle _               = False

hasCont :: SecItem -> Bool
hasCont (Cont _) = True
hasCont _        = False

hasSect :: SecItem -> Bool
hasSect (Sect _) = True
hasSect _        = False

hasSent :: SecItem -> Bool
hasSent (Sent _) = True
hasSent _        = False

hasUQI :: SecItem -> Bool
hasUQI (UnQuantI _) = True
hasUQI _            = False

hasUQO :: SecItem -> Bool
hasUQO (UnQuantO _) = True
hasUQO _            = False

hasDDef :: SecItem -> Bool
hasDDef (DataDef _) = True
hasDDef _           = False

hasIMods :: SecItem -> Bool
hasIMods (IMods _) = True
hasIMods _         = False

hasTMods :: SecItem -> Bool
hasTMods (TMods _) = True
hasTMods _         = False

-----------------
-- GET SECITEM --
-----------------

getItem :: (a->Bool) -> [a] -> Maybe a
getItem func ls = find (func) ls

getTitleize :: (Maybe SecItem) -> Bool
getTitleize (Just (SingularTitle)) = True
getTitleize (Just _)               = False
getTitleize Nothing                = False

getSection :: (Maybe SecItem) -> [Section]
getSection (Just (Sect xs)) = xs
getSection (Just _) = []
getSection Nothing = []

getSecContents :: (Maybe SecItem) -> [Contents]
getSecContents (Just (Cont xs)) = xs
getSecContents (Just _) = []
getSecContents Nothing = []

getSent :: (Maybe SecItem) -> [Sentence]
getSent (Just (Sent xs)) = xs
getSent (Just _)         = []
getSent Nothing          = []

getUQO :: (Maybe SecItem) -> [UncertQ]
getUQO (Just (UnQuantO xs)) = xs
getUQO (Just _)             = []
getUQO Nothing              = []

getUQI :: (Maybe SecItem) -> [UncertQ]
getUQI (Just (UnQuantI xs)) = xs
getUQI (Just _)             = []
getUQI Nothing              = []

getDDef :: (Maybe SecItem) -> [QDefinition]
getDDef (Just (DataDef xs)) = xs
getDDef (Just _)            = []
getDDef Nothing             = []

getIMods :: (Maybe SecItem) -> [RelationConcept]
getIMods (Just (IMods xs))   = xs
getIMods (Just _)            = []
getIMods Nothing             = []

getTMods :: (Maybe SecItem) -> [RelationConcept]
getTMods (Just (TMods xs))   = xs
getTMods (Just _)            = []
getTMods Nothing             = []

----------------------------
-- PULL SECITEM FROM LIST --
----------------------------

pullFunc :: [SecItem] -> (Maybe SecItem -> t) -> (SecItem -> Bool) -> t
pullFunc xs f g = f (getItem g xs)

pullTitle :: NamedIdea a => [SecItem] -> a -> Sentence
pullTitle xs = boolTitle $ pullFunc xs getTitleize hasTitle

boolTitle :: NamedIdea a => Bool -> (a -> Sentence)
boolTitle True  = titleize
boolTitle False = titleize' 

pullSections :: [SecItem] -> [Section]
pullSections xs = pullFunc xs getSection hasSect

pullContents :: [SecItem] -> [Contents]
pullContents xs = pullFunc xs getSecContents hasCont

pullSents :: [SecItem] -> [Sentence]
pullSents xs = pullFunc xs getSent hasSent

pullUQI :: [SecItem] -> [UncertQ]
pullUQI xs = pullFunc xs getUQI hasUQI

pullUQO :: [SecItem] -> [UncertQ]
pullUQO xs = pullFunc xs getUQO hasUQO

pullDDefs :: [SecItem] -> [QDefinition]
pullDDefs xs = pullFunc xs getDDef hasDDef

pullIMods :: [SecItem] -> [RelationConcept]
pullIMods xs = pullFunc xs getIMods hasIMods

pullTMods :: [SecItem] -> [RelationConcept]
pullTMods xs = pullFunc xs getTMods hasTMods

--getID :: SubSec -> String
--getID (SectionModel niname _) = niname ^. uid

--pullSubSec :: (NamedIdea a) => a -> [SubSec] -> Maybe SubSec
--pullSubSec nameid ls = getItem (\x -> (getID x) == (nameid ^. uid)) ls

-----------------------
-- Section Assembler --
-----------------------

assembler :: (Idea c, HasSymbolTable s) => c -> s -> SubSec -> [SubSec] -> Section
assembler progName symMap thisSection subsecs = 
  (sectionMap progName thisSection) subsections
  where subsections = map (render progName symMap) subsecs 

sectionMap :: Idea c => c -> SubSec -> [Section] -> Section
sectionMap progName (SectionModel niname xs)
  |  compareID niname (Doc.solutionCharSpec ^. uid)         = SRS.solCharSpec
    [scsIntro progName]
  | compareID niname  (Doc.problemDescription ^. uid)       = SRS.probDesc
    [problemDescriptionIntro progName (pullSents xs)]
  | compareID niname  (Doc.generalSystemDescription ^. uid) = SRS.genSysDes
    [genSysIntro]
  | compareID niname  (Doc.requirement ^. uid)              = SRS.require
    [requirementsIntro]
  | otherwise                                              = error "no matches on section name"

--------------------
-- Section Render --
--------------------

render :: (Idea c, HasSymbolTable s) => c -> s -> SubSec -> Section
render progName symMap item@(SectionModel niname _)
  | compareID niname (Doc.assumption ^. uid)       = assumptionSect        item
  | compareID niname (Doc.thModel ^. uid)          = theoreticalModelSect  item symMap progName
  | compareID niname (Doc.genDefn ^. uid)          = generalDefinitionSect item symMap
  | compareID niname (Doc.inModel ^. uid)          = instanceModelSect     item symMap
  | compareID niname (Doc.dataDefn ^. uid)         = dataDefinitionSect    item symMap
  | compareID niname (Doc.dataConst ^. uid)        = dataConstraintSect    item 
  | compareID niname (Doc.termAndDef ^. uid)       = termDefinitionSect    item
  | compareID niname (Doc.goalStmt ^. uid)         = goalStatementSect     item
  | compareID niname (Doc.systemConstraint ^. uid) = systemConstraintSect  item
  | otherwise                                      = genericSect           item

------------------------------
-- Section Render Functions --
------------------------------

genericSect :: SubSec -> Section
genericSect (SectionModel niname xs) = section'' (pullTitle xs niname) 
  (pullContents xs) (pullSections xs) (niname ^. uid)

------------------------------------------------
-- GENERAL SYSTEM DESCRIPTION SECTION BUILDER --
------------------------------------------------

systemConstraintSect :: SubSec -> Section
systemConstraintSect (SectionModel _ xs) = SRS.sysCon
  ((systemConstraintIntro (pullSents xs)):(pullContents xs)) (pullSections xs)

-------------------------------------------------
-- Specific System Description SECTION BUILDER --
-------------------------------------------------

termDefinitionSect :: SubSec -> Section
termDefinitionSect (SectionModel _ xs) = SRS.termAndDefn
  ((termDefinitionIntro (pullSents xs)):(pullContents xs)) (pullSections xs)

goalStatementSect :: SubSec -> Section
goalStatementSect (SectionModel _ xs) = SRS.goalStmt
  ((goalStatementIntro (pullSents xs)):(pullContents xs)) (pullSections xs)

-----------------------------------------------------------
-- Solution Characteristic Specification SECTION BUILDER --
-----------------------------------------------------------

assumptionSect :: SubSec -> Section
assumptionSect (SectionModel _ xs) = SRS.assumpt
  (assumpIntro:(pullContents xs)) (pullSections xs)


theoreticalModelSect :: (Idea a, HasSymbolTable s) => SubSec -> s -> a -> Section
theoreticalModelSect (SectionModel _ xs) _ progName = SRS.thModel
 ((tModIntro progName):theoreticalModels ++ 
  (pullContents xs)) (pullSections xs)
  where theoreticalModels = map symMap $ pullTMods xs
        symMap            = Definition . Theory


generalDefinitionSect :: (HasSymbolTable s) => SubSec -> s -> Section
generalDefinitionSect (SectionModel _ xs) _ = SRS.genDefn
  (generalDefsIntro:contents) (pullSections xs)
  where generalDefsIntro = generalDefinitionIntro contents
        contents         = (pullContents xs)


instanceModelSect :: (HasSymbolTable s) => SubSec -> s -> Section
instanceModelSect (SectionModel _ xs) _ = SRS.inModel
  (iModIntro:instanceModels ++ (pullContents xs)) (pullSections xs)
  where symMap         = Definition . Theory
        instanceModels = map symMap $ pullIMods xs


dataDefinitionSect :: (HasSymbolTable s) => SubSec -> s -> Section
dataDefinitionSect (SectionModel _ xs) _ = SRS.dataDefn
  (dataIntro:dataDefinitions ++ (pullContents xs)) (pullSections xs)
  where dataIntro       = dataDefinitionIntro $ pullSents xs
        symMap          = Definition . Data
        dataDefinitions = map symMap $ pullDDefs xs


dataConstraintSect :: SubSec -> Section
dataConstraintSect (SectionModel _ xs) = SRS.datCon
  ([dataConIntro, inputTable, outputTable] ++ (pullContents xs)) (pullSections xs)
  where dataConIntro = dataConstraintParagraph (pullContents xs) (pullSents xs)
        inputTable  = inDataConstTbl $ pullUQI xs
        outputTable = outDataConstTbl $ pullUQO xs

--FIXME generate tables here
--

--------------------------------------------
-- CONTENT BUILDING FUNCTIONS & CONSTANTS --
--------------------------------------------

--------------------------------
-- GENERAL SYSTEM DESCRIPTION --
--------------------------------


--------------------------
-- USER CHARACTERISTICS --
--------------------------


------------------------
-- SYSTEM CONSTRAINTS --
------------------------

systemConstraintIntro :: [Sentence] -> Contents
systemConstraintIntro [] = Paragraph (S "There are no" +:+.
  plural Doc.systemConstraint)
systemConstraintIntro l = foldlSP l


---------------------------------
-- SPECIFIC SYSTEM DESCRIPTION --
---------------------------------

-------------------------
-- PROBLEM DESCRIPTION --
-------------------------

problemDescriptionIntro :: Idea c => c -> [Sentence] -> Contents
problemDescriptionIntro progName []       = problemDescriptionSent progName
  EmptyS EmptyS
problemDescriptionIntro _ [x]      = Paragraph x
problemDescriptionIntro progName (x:y:_) = problemDescriptionSent progName x y

problemDescriptionSent :: Idea c => c -> Sentence -> Sentence -> Contents
problemDescriptionSent progName start end = foldlSP [start, (short progName), 
  S "is a", (phrase computer), (phrase program), S "developed to", end]

--------------------------
-- TERM AND DEFINITIONS --
--------------------------

termDefinitionIntro :: [Sentence] -> Contents
termDefinitionIntro end = Paragraph $ foldle (+:+) (+:+) (EmptyS)
  [S "This subsection provides a list of terms",
  S "that are used in the subsequent", plural Doc.section_, S "and their",
  S "meaning, with the", phrase Doc.purpose, S "of reducing ambiguity",
  S "and making it easier to correctly understand the", plural Doc.requirement, 
  foldlSent end]

--------------------
-- GOAL STATEMENT --
--------------------

goalStatementIntro :: [Sentence] -> Contents
goalStatementIntro inputs = Paragraph $ foldl (+:+) EmptyS [S "Given", 
  (inputToSystem inputs), plural Doc.goalStmt +: S "are"]
  where inputToSystem [] = S "the inputs" `sC` S "the" --FIXME add ref input variables if none are given?
        inputToSystem listInputs = (foldlList listInputs) `sC` S "the"


-------------------------------------------
-- SOLUTION CHARACTERISTIC SPECIFICATION --
-------------------------------------------

scsIntro :: (Idea c) => c -> Contents
scsIntro progName = foldlSP [S "The", plural Doc.inModel, 
  S "that govern", short progName, S "are presented in" +:+. 
  S "FIXME REF to IModSection", S "The", phrase Doc.information, S "to understand", 
  (S "meaning" `ofThe` plural Doc.inModel), 
  S "and their derivation is also presented, so that the", plural Doc.inModel, 
  S "can be verified"]


-----------------
-- ASSUMPTIONS --
-----------------

-- takes a bunch of references to things discribed in the wrapper
assumpIntro :: Contents
assumpIntro = Paragraph $ foldlSent 
  [S "This", (phrase Doc.section_), S "simplifies the original", 
  (phrase Doc.problem), S "and helps in developing the", (phrase Doc.thModel), 
  S "by filling in the missing", (phrase Doc.information), S "for the" +:+. 
  (phrase Doc.physicalSystem), S "The numbers given in the square brackets refer to the", 
  foldr1 sC (map refs itemsAndRefs) `sC` S "or", 
  refs (Doc.likelyChg) `sC` S "in which the respective", 
  (phrase Doc.assumption), S "is used"] --FIXME: use some clever "zipWith"
  where refs chunk = (titleize' chunk) {--+:+ sSqBr (makeRef ref)--} 
        itemsAndRefs = [Doc.thModel, Doc.genDefn, Doc.dataDefn, Doc.inModel] --FIXME ADD REFS BACK

------------------------
-- THEORETICAL MODELS --
------------------------

tModIntro :: (Idea a) => a -> Contents
tModIntro progName = foldlSP [S "This", phrase Doc.section_, S "focuses on",
  S "the", phrase Doc.general, (plural equation) `sAnd` (plural law), S "that",
  short progName, S "is based on"]

-------------------------
-- GENERAL DEFINITIONS --
-------------------------

generalDefinitionIntro :: (Referable t) => [t] -> Contents
generalDefinitionIntro [] = Paragraph $ S "There are no general definitions."
generalDefinitionIntro _ = foldlSP [S "This", phrase Doc.section_, 
  S "collects the", (plural law) `sAnd` (plural equation), 
  S "that will be used in deriving the", 
  plural Doc.dataDefn `sC` S "which in turn are used to build the", 
  plural Doc.inModel]

----------------------
-- DATA DEFINITIONS --
----------------------

dataDefinitionIntro :: [Sentence] -> Contents
dataDefinitionIntro xs = Paragraph $ (foldlSent [S "This", phrase Doc.section_, 
    S "collects and defines all the", plural Doc.datum, 
    S "needed to build the", plural Doc.inModel] +:+ foldl (+:+) EmptyS xs)


---------------------
-- INSTANCE MODELS --
---------------------

-- just need to provide the four references in order to this function. Nothing can be input into r4 if only three tables are present
iModIntro :: Contents
iModIntro = foldlSP [S "This", phrase Doc.section_, 
  S "transforms the", phrase Doc.problem, S "defined in", S "FIXME REF", 
  S "into one which is expressed in mathematical terms. It uses concrete", 
  plural Doc.symbol_, S "defined in", S "FIXME REF", 
  S "to replace the abstract", plural Doc.symbol_, S "in the", 
  plural Doc.model, S "identified in", S "FIXME REF" :+: S " and" +:+ S "FIXME REF"]
  
---------------------
-- DATA CONSTRAINTS --
---------------------

-- reference to the input/ ouput tables -> optional middle sentence(s) (use EmptyS if not wanted) -> 
-- True if standard ending sentence wanted -> optional trailing sentence(s) -> Contents
dataConstraintParagraph :: [Contents] -> [Sentence] -> Contents
dataConstraintParagraph tableRef [] = Paragraph $ 
  (dataConstraintIntroSent tableRef) +:+ (dataConstraintClosingSent [EmptyS])
dataConstraintParagraph tableRef (mid:xs) = Paragraph $
  (dataConstraintIntroSent tableRef) +:+ mid +:+ 
  (dataConstraintClosingSent xs)

dataConstraintIntroSent :: [Contents] -> Sentence
dataConstraintIntroSent tableRef = foldlSent [(listofTablesToRefs tableRef), 
  S "the", plural Doc.datumConstraint, S "on the", phrase Doc.input_
  `sAnd` phrase Doc.output_ +:+. (plural Doc.variable `sC` S "respectively"), 
  S "The", phrase Doc.column, S "for", phrase Doc.physical, 
  plural Doc.constraint, S "gives the", phrase Doc.physical, 
  plural Doc.limitation, S "on the range of", plural Doc.value, 
  S "that can be taken by the", phrase Doc.variable]


dataConstraintClosingSent :: [Sentence] -> Sentence
dataConstraintClosingSent trailing = (foldlSent
  [S "The", plural Doc.constraint, S "are conservative, to give", 
  (phrase Doc.user `ofThe` phrase Doc.model), S "the flexibility to", 
  S "experiment with unusual situations. The", phrase Doc.column, 
  S "of typical", plural Doc.value, 
  S "is intended to provide a feel for a common scenario"])
  +:+ dataConstraintUncertainty +:+ S "FIXME" +:+ (foldl (+:+) EmptyS trailing) 
  --FIXME make uncertainty specificiable 

dataConstraintUncertainty :: Sentence
dataConstraintUncertainty = foldlSent [S "The", phrase Doc.uncertainty, 
  phrase Doc.column, S "provides an estimate of the confidence with which the", 
  phrase Doc.physical, plural Doc.quantity +:+. S "can be measured", 
  S "This", phrase Doc.information, S "would be part of the", phrase Doc.input_, 
  S "if one were performing an", phrase Doc.uncertainty, S "quantification exercise"]

------------------
-- REQUIREMENTS --
------------------

requirementsIntro :: Contents
requirementsIntro = foldlSP
  [S "This", (phrase Doc.section_), S "provides the",
  (plural Doc.functionalRequirement) `sC` S "the business tasks that the",
  (phrase Doc.software), S "is expected to complete, and the", 
  (plural Doc.nonfunctionalRequirement) `sC` S "the qualities that the",
  (phrase Doc.software), S "is expected to exhibit"]

---------------------------------
-- NON-FUNCTIONAL REQUIREMENTS --
---------------------------------


-----------------------------
-- FUNCTIONAL REQUIREMENTS --
-----------------------------
