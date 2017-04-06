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
  TSymb :: Contents -> RefTab
  TSymb' :: LFunc -> Contents -> RefTab
  --FIXME: Pull out Contents as it's currently a verbatim "intro" to TSymb.
  TAandA :: RefTab
  TVerb :: Section -> RefTab
  -- add more here
data RefSec = RefProg Contents [RefTab] | RefVerb Section -- continue
data DocSection = Verbatim Section | RefSec RefSec

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
  ((titleize kind) +:+ S "for" +:+ (sys^.term))
  (manyNames authors) (mkSections si l)

--When we want to use the short form for titles.  
mkDoc' :: DocDesc -> SystemInformation -> Document
mkDoc' l si@(SI sys kind authors _ _ _ _) = Document 
  ((titleize kind) +:+ S "for" +:+ (short sys))
  (manyNames authors) (mkSections si l)

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
    mkSubRef (SI _ _ _ _ v _ _) (TSymb con) l' = 
      (Section (titleize tOfSymb) (map Con [con, (table (sort v) (^.term))])) : l'
    mkSubRef (SI _ _ _ _ _ cccs _) (TSymb' f con) l' = (mkTSymb cccs f con) : l'
    mkSubRef (SI _ _ _ _ v cccs n) TAandA l' = (table_of_abb_and_acronyms $ 
      filter (isJust . getA) (map nw v ++ map nw cccs ++ map nw n)) : l'
    mkSubRef _              (TVerb s) l' = s : l'

mkTSymb :: (Quantity e, Concept e, Ord e) => 
  [e] -> LFunc -> Contents -> Section
mkTSymb v f c = Section (titleize tOfSymb) (map Con [c, table (sort v) (lf f)])
  where lf Term = (^.term)
        lf Defn = (^.defn)
        lf (TermExcept cs) = (\x -> if (x ^. id) `elem` (map (^.id) cs) then
          (x ^. defn) else (x ^. term)) --Compare chunk ids, since we don't
          --actually care about the chunks themselves in LFunc.
        lf (DefnExcept cs) = (\x -> if (x ^. id) `elem` (map (^.id) cs) then
          (x ^. term) else (x ^. defn))

--tsymb constructor
tsymb, tsymb' :: Contents -> RefTab
tsymb intro = TSymb intro                --Default Term
tsymb' intro = TSymb' Defn intro         --Default Defn
tsymb'' :: Contents -> LFunc -> RefTab
tsymb'' intro lfunc = TSymb' lfunc intro --Custom