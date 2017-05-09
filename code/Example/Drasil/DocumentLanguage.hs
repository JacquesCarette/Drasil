{-# LANGUAGE GADTs #-}
module Drasil.DocumentLanguage where

import Language.Drasil

import Control.Lens ((^.))

import Drasil.TableOfUnits (table_of_units)
import Drasil.TableOfSymbols (table)
import Drasil.TableOfAbbAndAcronyms (table_of_abb_and_acronyms)

import Data.Drasil.Concepts.Documentation (refmat, tOfSymb)

import Data.Maybe (isJust)
import Data.List (sort)
import Prelude hiding (id)

---------------------------------------------------------------------------
-- Start the process of moving away from Document as the main internal
-- representation of information, to something more informative.
-- Over time, we'll want to have a cleaner separation, but doing that
-- all at once would break too much for too long.  So we start here
-- instead.

type System = Sentence
type DocKind = Sentence

data SystemInformation where
--FIXME:
--There should be a way to remove redundant "Quantity" constraint.
-- I'm thinking for getting concepts that are also quantities, we could
-- use a lookup of some sort from their internal (Drasil) ids.
-- FIXME: b shouldn't need to be a NounPhrase, this will be fixed after
-- NP is built into NamedIdea.
 SI :: (NamedIdea a, NamedIdea b, NounPhrase b, HasName c, Unit d,
  Quantity e, Ord e, Ord f, Quantity f, Concept f, NamedIdea g) => {
  _sys :: a,
  _kind :: b,
  _authors :: [c],
  _units :: [d],
  _quants :: [e],
  _concepts :: [f],
  _namedIdeas :: [g]
  } -> SystemInformation

-- anything with 'Verb' in it should eventually go
data RefTab where 
  TUnits :: RefTab
  TUnits' :: [TUIntro] -> RefTab -- Customized intro
  TSymb :: [TSIntro] -> RefTab
  TSymb' :: LFunc -> [TSIntro] -> RefTab
  TAandA :: RefTab
  TVerb :: Section -> RefTab
  -- add more here

data RefSec = RefProg Contents [RefTab] | RefVerb Section -- continue
data DocSection = Verbatim Section | RefSec RefSec

data TSIntro = TypogConvention [TConvention]
             | SymbOrder
             | SymbConvention [Literature]
             | TSPurpose
             
data TConvention = Vector Emphasis
                 | Verb Sentence
                 
data Emphasis = Bold
              | Italics

instance Show Emphasis where
  show Bold = "bold"
  show Italics = "italics"

data Literature = Lit Topic
                | Doc Topic
                | Manual Topic

type Topic = NWrapper

data TUIntro = System
             | Derived
             | TUPurpose

-- Lens (lookup) functions (currently for TSymb)

data LFunc where
  Term :: LFunc
  Defn :: LFunc
  TermExcept :: Concept c => [c] -> LFunc
  DefnExcept :: Concept c => [c] -> LFunc

type DocDesc = [DocSection]

-- 
mkDoc :: DocDesc -> SystemInformation -> Document
mkDoc l si@(SI sys kind authors _ _ _ _) = Document 
  (kind `for` sys) (manyNames authors) (mkSections si l)

--When we want to use the short form for titles.  
mkDoc' :: DocDesc -> SystemInformation -> Document
mkDoc' l si@(SI sys kind authors _ _ _ _) = Document 
  (kind `for''` sys) (manyNames authors) (mkSections si l)

mkSections :: SystemInformation -> DocDesc -> [Section]
mkSections si l = foldr doit [] l
  where
    doit :: DocSection -> [Section] -> [Section]
    doit (Verbatim s) ls = s : ls
    doit (RefSec rs)  ls = mkRefSec si rs : ls

mkRefSec :: SystemInformation -> RefSec -> Section
mkRefSec _  (RefVerb s) = s
mkRefSec si (RefProg c l) = section (titleize refmat) c (foldr (mkSubRef si) [] l)
  where
    mkSubRef :: SystemInformation -> RefTab -> [Section] -> [Section]
    mkSubRef (SI _ _ _ u _ _ _)  TUnits   l' = table_of_units u : l'
    mkSubRef (SI _ _ _ u _ _ _) (TUnits' con) l' = error "Not yet implemented" 
    mkSubRef (SI _ _ _ _ v _ _) (TSymb con) l' = 
      (Section (titleize tOfSymb) 
      (map Con [tsIntro con, (table (sort v) (\x -> phrase $ x ^.term))])) : l'
    mkSubRef (SI _ _ _ _ _ cccs _) (TSymb' f con) l' = (mkTSymb cccs f con) : l'
    mkSubRef (SI _ _ _ _ v cccs n) TAandA l' = (table_of_abb_and_acronyms $ 
      filter (isJust . getA) (map nw v ++ map nw cccs ++ map nw n)) : l'
    mkSubRef _              (TVerb s) l' = s : l'

mkTSymb :: (Quantity e, Concept e, Ord e) => 
  [e] -> LFunc -> [TSIntro] -> Section
mkTSymb v f c = Section (titleize tOfSymb) (map Con [tsIntro c, table (sort v) (lf f)])
  where lf Term = (\x -> phrase $ x ^. term)
        lf Defn = (^. defn)
        lf (TermExcept cs) = (\x -> if (x ^. id) `elem` (map (^. id) cs) then
          (x ^. defn) else (phrase $ x ^. term)) --Compare chunk ids, since we don't
          --actually care about the chunks themselves in LFunc.
        lf (DefnExcept cs) = (\x -> if (x ^. id) `elem` (map (^.id) cs) then
          (phrase $ x ^. term) else (x ^. defn))

--tsymb constructor
tsymb, tsymb' :: [TSIntro] -> RefTab
tsymb intro = TSymb intro                --Default Term
tsymb' intro = TSymb' Defn intro         --Default Defn

tsymb'' :: [TSIntro] -> LFunc -> RefTab
tsymb'' intro lfunc = TSymb' lfunc intro --Custom

-- table of symbols intro builder
tsIntro :: [TSIntro] -> Contents
tsIntro x = Paragraph $ foldr (+:+) (S "") (map tsI x)

tsI :: TSIntro -> Sentence
tsI (TypogConvention ts) = typogConvention ts
tsI SymbOrder = S "The symbols are listed in alphabetical order."
tsI (SymbConvention ls) = symbConvention ls
tsI TSPurpose = S "The table that follows summarizes the symbols used in" +:+
  S "this document along with their units."

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

symbConvention :: [Literature] -> Sentence
symbConvention [] = error "Attempting to reference no literature for SymbConvention"
symbConvention scs = S "The choice of symbols was made to be consistent with the" +:+
                      makeSentence scs
  where makeSentence (x:[]) = scon x :+: S "."
        makeSentence (x:y:[]) = scon x +:+ S "and with" +:+. scon y
        makeSentence (x:y:z:[]) = scon x `sC` scon y `sC` S "and" +:+. scon z
        makeSentence (x:xs) = scon x `sC` makeSentence xs
        makeSentence _ = error "How did you get here?"
        scon (Lit x) = phrase (x ^. term) +:+ S "literature"
        scon (Doc x) = S "existing documentation for" +:+ (phrase $ x ^. term)
        scon (Manual x) = S "that used in the" +:+ (phrase $ x ^. term) +:+ S "manual"
  
tuIntro :: [TUIntro] -> Contents
tuIntro x = Paragraph $ foldr (+:+) (S "") (map tuI x)

tuI :: TUIntro -> Sentence
tuI System  = (S "The unit system used throughout is SI (Syst" :+: 
  (F Grave 'e') :+: S "me International d'Unit" :+: (F Acute 'e') :+: S "s).")
tuI TUPurpose = S "For each unit, the table lists the symbol," +:+
  S "a description and the SI name."
tuI Derived = S "In addition to the basic units, several derived units are" +:+ 
  S "also used."
