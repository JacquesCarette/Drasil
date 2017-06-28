module Drasil.Sections.SolutionCharacterSpec
  (
  sSubSec,
  genericSect,
  siCon,
  siSect,
  siTMod,
  siIMod,
  siDDef,
  siSent,
  siTitl
  ) where

import Language.Drasil
--import Data.Drasil.Concepts.Math (equation)
--import Data.Drasil.Concepts.Software (program)
--import Data.Drasil.Utils (foldle)
--import Data.Drasil.SentenceStructures
import qualified Data.Drasil.Concepts.Documentation as Doc
import Data.List (find)

data SecItem = Cont [Contents]
             | Sect [Section]
             | TMods [RelationConcept]
             | IMods [RelationConcept]
             | DataDef [QDefinition]
             | GenDef [RelationConcept]
             | Sent [Sentence]
             | TitleFunc (CI -> Sentence)

data SolSubSec = SectionModel CI [SecItem]


sSubSec :: CI -> [SecItem] -> SolSubSec
sSubSec name xs = SectionModel name xs

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

siTitl :: (CI -> Sentence) -> SecItem
siTitl f = TitleFunc f

{--renderSec item@(SectionModel name xs) | name == Doc.assumption      = 0 --assumptionSect item
                                      | name == Doc.thModel         = 0 --theoreticalModelSect item
                                      | name == Doc.genDefn         = 0 --generalDefinitionSect item
                                      | name == Doc.inModel         = 0 --instanceModelSect item
                                      | name == Doc.dataDefn        = 0 --dataDefinitionSect item
                                      | name == Doc.datumConstraint = 0 --dataConstraintSect item
                                      | otherwise                   = 0 --genericSect item
--}
----------------------
--  HELPER FUNCTION --
----------------------

hasTitle :: SecItem -> Bool
hasTitle (TitleFunc _) = True
hasTitle _             = False

hasCont :: SecItem -> Bool
hasCont (Cont _) = True
hasCont _        = False

hasSect :: SecItem -> Bool
hasSect (Sect _) = True
hasSect _        = False

getSecItem :: (a->Bool) -> [a] -> Maybe a
getSecItem func ls = find (func) ls

getTitleize :: (Maybe SecItem) -> (CI -> Sentence)
getTitleize (Just (TitleFunc a)) = a
getTitleize (Just _) = titleize
getTitleize Nothing = titleize

getSections :: (Maybe SecItem) -> [Section]
getSections (Just (Sect xs)) = xs
getSections (Just _) = []
getSections Nothing = []

getSecContents :: (Maybe SecItem) -> [Contents]
getSecContents (Just (Cont xs)) = xs
getSecContents (Just _) = []
getSecContents Nothing = []

pullFunc :: [SecItem] -> (Maybe SecItem -> t) -> (SecItem -> Bool) -> t
pullFunc xs f g = f (getSecItem g xs)

pullTitle :: [SecItem] -> CI -> Sentence
pullTitle xs = pullFunc xs getTitleize hasTitle

pullSections :: [SecItem] -> [Section]
pullSections xs = pullFunc xs getSections hasSect

pullContents :: [SecItem] -> [Contents]
pullContents xs = pullFunc xs getSecContents hasCont

genericSect :: SolSubSec -> Section
genericSect (SectionModel name xs) = section ((pullTitle xs) name) (pullContents xs)
  (pullSections xs)



--solutionCharacterSpecificationAssembler :: (NamedIdea a) => a -> [SolSubSec] -> Section

{--solutionCharactersticCon :: (NamedIdea a) => a -> SOLsec -> [Section] -> Section
solutionCharactersticCon progName (Sect as tm gd dd im dc) xs = SRS.solCharSpec
  [solutionCharSpecIntro progName instanceModels] (subsections)
  where assumptions        = assumptionSub        as
          theoreticalModels generalDefinitions dataDefinitions instanceModels 
        theoreticalModels  = theoreticalModelSub  tm progName
        generalDefinitions = generalDefinitionSub gd
        dataDefinitions    = dataDefinitionSub    dd
        instanceModels     = instanceModelSub     im 
          dataDefinitions theoreticalModels generalDefinitions
        dataConstraints    = dataConstraintSub    dc
        subsections = [assumptions, theoreticalModels, generalDefinitions,
                       dataDefinitions, instanceModels, dataConstraints] ++ xs

assumptionSub :: SOLsub -> Section -> Section -> Section -> Section -> Section
assumptionSub (Subs _ _ _ sectionRef ys) ref1 ref2 ref3 ref4 = SRS.assump
  ((assumpIntro ref1 ref2 ref3 ref4 sectionRef):ys) []

theoreticalModelSub :: (NamedIdea a) => SOLsub -> a -> Section
theoreticalModelSub (Subs _ _ _ _ ys) progName = SRS.thModel ((thModIntro progName):ys) []

generalDefinitionSub :: SOLsub -> Section
generalDefinitionSub (Subs _ _ _ _ ys) = SRS.genDefn (generalDefinitionIntro ys:ys) []

dataDefinitionSub :: SOLsub -> Section
dataDefinitionSub (Subs _ _ ending _ ys) = SRS.dataDefn ((dataDefinitionIntro ending):ys) []

instanceModelSub :: SOLsub -> Section -> Section -> Section -> Section
instanceModelSub (Subs _ _ _ sectionRef ys) ref1 ref2 ref3 = SRS.inModel ((introContent):ys) []
  where introContent = inModelIntro sectionRef ref1 ref2 ref3

dataConstraintSub :: SOLsub -> Section
dataConstraintSub (Subs uncertain mid trail _ ys) = SRS.datCon ((dataContent):ys) []
  where dataContent = dataConstraintParagraph uncertain (listofTablesToRefs ys) mid trail


solutionCharSpecIntro :: (NamedIdea a) => a -> Section -> Contents
solutionCharSpecIntro progName instModelSection = foldlSP [S "The", plural inModel, 
  S "that govern", short progName, S "are presented in" +:+. 
  makeRef (instModelSection), S "The", phrase information, S "to understand", 
  (S "meaning" `ofThe` plural inModel), 
  S "and their derivation is also presented, so that the", plural inModel, 
  S "can be verified"]


-- takes a bunch of references to things discribed in the wrapper
assumpIntro :: (LayoutObj t) => t -> t -> t -> t -> t -> [t] -> Contents
assumpIntro r1 r2 r3 r4 r5 genDef = Paragraph $ foldlSent 
          [S "This", (phrase section_), S "simplifies the original", (phrase problem), 
          S "and helps in developing the", (phrase thModel), S "by filling in the", 
          S "missing", (phrase information), S "for the" +:+. (phrase physicalSystem), 
          S "The numbers given in the square brackets refer to the", 
          foldr1 sC (map (refs) (itemsAndRefs)) `sC` S "or", 
          refs (likelyChg, r5) `sC` S "in which the respective", 
          (phrase assumption), S "is used"] --FIXME: use some clever "zipWith"
          where refs (chunk, ref) = (titleize' chunk) +:+ sSqBr (makeRef ref) 
                itemsAndRefs = [(thModel, r1), (genDefn, r2), (dataDefn, r3), 
                                (inModel, r4)]

[thModel, genDefn, dataDefn, inModel, likelyChg]

makeReferences r1 r2 r3 r4 r5 [] = 



assumptionParagraph ::  Sentence -> Contents
assumptionParagraph references = foldSP [S "This", (phrase section_), 
  S "simplifies the original", (phrase problem), S "and helps in developing the",
  (phrase thModel), S "by filling in the", S "missing", (phrase information), 
  S "for the" +:+. (phrase physicalSystem), 
  S "The numbers given in the square brackets refer to the", references `sC` 
  S "in which the respective", (phrase assumption), S "is used"]

-- generalized theoretical model introduction: identifies key word pertaining to topic
tModIntro :: (NamedIdea a) => a -> Contents
tModIntro progName = foldlSP [S "This", phrase section_, S "focuses on",
  S "the", phrase general, (plural equation), S "and", S "laws that",
  short progName, S "is based on"]


generalDefinitionIntro :: (LayoutObj t) => [t] -> Contents
generalDefinitionIntro [] = Paragraph $ S "There are no general definitions."
generalDefinitionIntro _ = foldlSP [S "This", phrase section_, 
  S "collects the", S "laws and", (plural equation), 
  S "that will be used in", S "deriving the", 
  plural dataDefn `sC` S "which in turn are used to", S "build the", 
  plural inModel]

dataDefinitionIntro :: Sentence -> Contents
dataDefinitionIntro closingSent = Paragraph $ (foldlSent [S "This", phrase section_, 
    S "collects and defines all the", plural datum, 
    S "needed to build the", plural inModel] +:+ closingSent)

-- just need to provide the four references in order to this function. Nothing can be input into r4 if only three tables are present
iModIntro :: (LayoutObj t) -> t -> t -> t -> t -> [t] -> Contents
iModIntro r1 r2 r3 r4 genDef = foldlSP [S "This", phrase section_, 
  S "transforms the", phrase problem, S "defined in", (makeRef r1), 
  S "into one which is expressed in mathematical terms. It uses concrete", 
  plural symbol_, S "defined in", (makeRef r2), 
  S "to replace the abstract", plural symbol_, S "in the", 
  plural model, S "identified in", (makeRef r3) :+: end genDef]
    where end genDef = S " and" +:+ (makeRef r4)
          end [] = EmptyS


-- makes a list of references to tables takes
-- l  list of layout objects that can be referenced
-- outputs a sentence containing references to the layout objects 
listofTablesToRefs :: LayoutObj l => [l] -> Sentence
listofTablesToRefs  []     = EmptyS
listofTablesToRefs  [x]    = (makeRef x) +:+ S "shows"
listofTablesToRefs  [x,y]  = (makeRef x) `sC` S "and" +:+ listofTablesToRefs [y]
listofTablesToRefs  (x:xs) = (makeRef x) `sC` listofTablesToRefs (xs)

-- reference to the input/ ouput tables -> optional middle sentence(s) (use EmptyS if not wanted) -> 
-- True if standard ending sentence wanted -> optional trailing sentence(s) -> Contents
dataConstraintParagraph :: Sentence -> Sentence -> Sentence -> Sentence -> Contents
dataConstraintParagraph hasUncertainty tableRef middleSent trailingSent = Paragraph $
  (dataConstraintIntroSent tableRef) +:+ middleSent +:+ 
  (dataConstraintClosingSent hasUncertainty trailingSent)

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
  S "provides an", S "estimate of the confidence with which the", phrase physical,
  plural quantity +:+. S "can be measured", S "This", phrase information,
  S "would be part of the", phrase input_, S "if one were performing an",
  phrase uncertainty, S "quantification exercise"]
--}