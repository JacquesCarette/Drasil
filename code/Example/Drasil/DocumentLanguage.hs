{-# LANGUAGE GADTs #-}
---------------------------------------------------------------------------
-- | Start the process of moving away from Document as the main internal
-- representation of information, to something more informative.
-- Over time, we'll want to have a cleaner separation, but doing that
-- all at once would break too much for too long.  So we start here
-- instead.
module Drasil.DocumentLanguage where

import Language.Drasil

import Control.Lens ((^.))

import Drasil.Sections.TableOfUnits (table_of_units)
import Drasil.Sections.TableOfSymbols (table)
import Drasil.Sections.TableOfAbbAndAcronyms (table_of_abb_and_acronyms)
import qualified Drasil.SRS as SRS
import qualified Drasil.Sections.Introduction as Intro

import Data.Drasil.Concepts.Documentation (refmat, tOfSymb)

import Data.Maybe (isJust)
import Data.List (sort)
import Prelude hiding (id)

type System = Sentence
type DocKind = Sentence

-- anything with 'Verb' in it should eventually go
-- | Reference subsections
data RefTab where 
  TUnits :: RefTab
  TUnits' :: [TUIntro] -> RefTab -- Customized intro
  TSymb :: [TSIntro] -> RefTab
  TSymb' :: LFunc -> [TSIntro] -> RefTab
  TAandA :: RefTab
  TVerb :: Section -> RefTab
  -- add more here
  
-- | Introduction subsections
data IntroSub where
  IVerb    :: Section -> IntroSub
  IPurpose :: Sentence -> IntroSub
  IScope   :: Sentence -> Sentence -> IntroSub
  IChar    :: Sentence -> Sentence -> Sentence -> IntroSub
  IOrgSec  :: Sentence -> CI -> Section -> Sentence -> IntroSub

-- | Reference section. Contents are top level followed by a list of subsections.
-- RefVerb is used for including verbatim subsections
data RefSec = RefProg Contents [RefTab] | RefVerb Section -- continue

--FIXME: This needs to be updated for the requisite information in introductionF
-- | Introduction section. Contents are top level followed by a list of 
-- subsections. IntroVerb is used for including verbatim subsections.
data IntroSec = IntroProg Sentence Sentence [IntroSub] 
  -- ^ Temporary, will be modified once we've figured out more about the section.
              | IntroVerb Section

-- | Document sections are either Verbatim, Reference, or Introduction 
-- sections (for now!)
data DocSection = Verbatim Section | RefSec RefSec | IntroSec IntroSec

-- | For creating the table of symbols intro
data TSIntro = TypogConvention [TConvention] -- ^ Typographic conventions used
             | SymbOrder -- ^ Symbol ordering (defaults to alphabetical)
             | SymbConvention [Literature] -- ^ Symbol conventions match specified literature
             | TSPurpose -- ^ Purpose of the Table of Symbols

-- | Possible typographic conventions
data TConvention = Vector Emphasis -- ^ How vectors are emphasized
                 | Verb Sentence -- ^ Verbatim for specialized conventions
                 
data Emphasis = Bold
              | Italics

instance Show Emphasis where
  show Bold = "bold"
  show Italics = "italics"

-- | Types of literature
data Literature = Lit Topic -- ^ literature
                | Doc Topic -- ^ existing documentation for (singular topic)
                | Doc' Topic -- ^ existing documentation for (plural of topic)
                | Manual Topic -- ^ manual

type Topic = NWrapper

-- | For creating the table of units intro
data TUIntro = System -- ^ System of units (defaults to SI)
             | Derived -- ^ Sentence about derived units being used alongside SI
             | TUPurpose -- ^ Purpose of the table of units

-- | Lens (lookup) functions (currently for TSymb)
data LFunc where
  Term :: LFunc
  Defn :: LFunc
  TermExcept :: Concept c => [c] -> LFunc
  DefnExcept :: Concept c => [c] -> LFunc
  TAD :: LFunc --Term and Definition

type DocDesc = [DocSection]

-- | Creates a document from a document description and system information
mkDoc :: DocDesc -> SystemInformation -> Document
mkDoc l si@(SI sys kind authors _ _ _ _ _ _ _ _) = Document 
  (kind `for` sys) (manyNames authors) (mkSections si l)

-- | Similar to 'makeDoc', but for when we want to use the short form for titles.  
mkDoc' :: DocDesc -> (NWrapper -> NWrapper -> Sentence) -> SystemInformation -> Document
mkDoc' l comb si@(SI sys kind authors _ _ _ _ _ _ _ _) = Document 
  ((nw kind) `comb` (nw sys)) (manyNames authors) (mkSections si l)

-- | Helper for creating the document sections
mkSections :: SystemInformation -> DocDesc -> [Section]
mkSections si l = foldr doit [] l
  where
    doit :: DocSection -> [Section] -> [Section]
    doit (Verbatim s) ls = s : ls
    doit (RefSec rs)  ls = mkRefSec si rs : ls
    doit (IntroSec is) ls = mkIntroSec si is : ls

-- | Helper for creating the reference section and subsections
mkRefSec :: SystemInformation -> RefSec -> Section
mkRefSec _  (RefVerb s) = s
mkRefSec si (RefProg c l) = section (titleize refmat) [c] (foldr (mkSubRef si) [] l)
  where
    mkSubRef :: SystemInformation -> RefTab -> [Section] -> [Section]
    mkSubRef (SI _ _ _ u _ _ _ _ _ _ _)  TUnits   l' = table_of_units u (tuIntro defaultTUI) : l'
    mkSubRef (SI _ _ _ u _ _ _ _ _ _ _) (TUnits' con) l' = table_of_units u (tuIntro con) : l'
    mkSubRef (SI _ _ _ _ v _ _ _ _ _ _) (TSymb con) l' = 
      (Section (titleize tOfSymb) 
      (map Con [tsIntro con, (table (sort v) at_start)])) : l'
    mkSubRef (SI _ _ _ _ _ cccs _ _ _ _ _) (TSymb' f con) l' = (mkTSymb cccs f con) : l'
    mkSubRef (SI _ _ _ _ v cccs n _ _ _ _) TAandA l' = (table_of_abb_and_acronyms $ 
      filter (isJust . getA) (map nw v ++ map nw cccs ++ map nw n)) : l'
    mkSubRef _              (TVerb s) l' = s : l'

-- | Helper for creating the table of symbols
mkTSymb :: (Quantity e, Concept e, Ord e) => 
  [e] -> LFunc -> [TSIntro] -> Section
mkTSymb v f c = Section (titleize tOfSymb) (map Con [tsIntro c, table (sort v) (lf f)])
  where lf Term = at_start
        lf Defn = (^. defn)
        lf (TermExcept cs) = (\x -> if (x ^. id) `elem` (map (^. id) cs) then
          (x ^. defn) else (at_start x)) --Compare chunk ids, since we don't
          --actually care about the chunks themselves in LFunc.
        lf (DefnExcept cs) = (\x -> if (x ^. id) `elem` (map (^.id) cs) then
          (at_start x) else (x ^. defn))
        lf TAD = (\tDef -> titleize tDef :+: S ":" +:+ (tDef ^. defn))

-- | table of symbols constructor
tsymb, tsymb' :: [TSIntro] -> RefTab
tsymb intro = TSymb intro                -- ^ Default Term and given intro
tsymb' intro = TSymb' Defn intro         -- ^ Default Defn and given intro

-- | Custom table of symbols constructor
tsymb'' :: [TSIntro] -> LFunc -> RefTab
tsymb'' intro lfunc = TSymb' lfunc intro -- ^ Custom function and intro.

-- | table of symbols intro builder. Used by mkRefSec
tsIntro :: [TSIntro] -> Contents
tsIntro x = Paragraph $ foldr (+:+) (EmptyS) (map tsI x)

-- | table of symbols intro writer. Translates a TSIntro to a list of Sentences
tsI :: TSIntro -> Sentence
tsI (TypogConvention ts) = typogConvention ts
tsI SymbOrder = S "The symbols are listed in alphabetical order."
tsI (SymbConvention ls) = symbConvention ls
tsI TSPurpose = S "The table that follows summarizes the symbols used in" +:+
  S "this document along with their units."

-- | typographic convention writer. Translates a list of typographic conventions
-- to a sentence
typogConvention :: [TConvention] -> Sentence
typogConvention [] = error "No arguments given for typographic conventions"
typogConvention ts = S "Throughout the document" `sC` (makeSentence ts)
  where makeSentence (x:[]) = tcon x :+: S "."
        makeSentence (x:y:[]) = tcon x +:+ S "and" +:+. tcon y
        makeSentence (x:y:z:[]) = tcon x `sC` tcon y `sC` S "and" +:+. tcon z
        makeSentence (x:xs) = tcon x `sC` makeSentence xs
        makeSentence _ = error "How did you get here?"
        tcon (Vector emph) = S ("symbols in " ++ show emph ++ 
                                " will represent vectors, and scalars otherwise")
        tcon (Verb s) = s

-- | symbolic convention writer.
symbConvention :: [Literature] -> Sentence
symbConvention [] = error "Attempting to reference no literature for SymbConvention"
symbConvention scs = S "The choice of symbols was made to be consistent with the" +:+
                      makeSentence scs
  where makeSentence (x:[]) = scon x :+: S "."
        makeSentence (x:y:[]) = scon x +:+ S "and with" +:+. scon y
        makeSentence (x:y:z:[]) = scon x `sC` scon y `sC` S "and" +:+. scon z
        makeSentence (x:xs) = scon x `sC` makeSentence xs
        makeSentence _ = error "How did you get here?"
        scon (Lit x) = phrase x +:+ S "literature"
        scon (Doc x) = S "existing documentation for" +:+ (phrase x)
        scon (Doc' x)   = S "existing documentation for" +:+ (plural x)
        scon (Manual x) = S "that used in the" +:+ (phrase x) +:+ S "manual"

-- | Table of units intro builder. Used by mkRefSec
tuIntro :: [TUIntro] -> Contents
tuIntro x = Paragraph $ foldr (+:+) (EmptyS) (map tuI x)

-- | table of units intro writer. Translates a TUIntro to a Sentence.
tuI :: TUIntro -> Sentence
tuI System  = (S "The unit system used throughout is SI (Syst" :+: 
  (F Grave 'e') :+: S "me International d'Unit" :+: (F Acute 'e') :+: S "s).")
tuI TUPurpose = S "For each unit, the table lists the symbol," +:+
  S "a description and the SI name."
tuI Derived = S "In addition to the basic units, several derived units are" +:+ 
  S "also used."

-- | Default table of units intro contains the 
defaultTUI :: [TUIntro]
defaultTUI = [System, Derived, TUPurpose]

mkIntroSec :: SystemInformation -> IntroSec -> Section
mkIntroSec _ (IntroVerb s) = s
mkIntroSec si (IntroProg probIntro progDefn l) = 
  Intro.introductionSection probIntro progDefn $ foldr (mkSubIntro si) [] l
  where
    mkSubIntro :: SystemInformation -> IntroSub -> [Section] -> [Section]
    mkSubIntro _ (IVerb s) l' = s : l'
    mkSubIntro _ (IPurpose intro) l' = Intro.purposeOfDoc intro : l'
    mkSubIntro (SI sys _ _ _ _ _ _ _ _ _ _) (IScope main intendedPurp) l' = 
      Intro.scopeOfRequirements main sys intendedPurp : l'
    mkSubIntro (SI sys _ _ _ _ _ _ _ _ _ _) (IChar know understand appStandd) l' =
      Intro.charIntRdrF know understand sys appStandd (SRS.userChar [] []) : l'
    mkSubIntro _ (IOrgSec i b s t) l' = Intro.orgSec i b s t : l'
    -- FIXME: s should be "looked up" using "b" once we have all sections being generated
