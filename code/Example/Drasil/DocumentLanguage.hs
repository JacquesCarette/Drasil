{-# LANGUAGE GADTs #-}
module Drasil.DocumentLanguage where

import Language.Drasil

import Control.Lens ((^.))

import Drasil.TableOfUnits (table_of_units)
import Drasil.TableOfSymbols (table)

import Data.Drasil.Concepts.Documentation (refmat, tsymb)

---------------------------------------------------------------------------
-- Start the process of moving away from Document as the main internal
-- representation of information, to something more informative.
-- Over time, we'll want to have a cleaner separation, but doing that
-- all at once would break too much for too long.  So we start here
-- instead.

type System = Sentence
type DocKind = Sentence

data SystemInformation where
 SI :: (Concept a, Concept b, HasName c, Unit d, SymbolForm e, Quantity e) => {
  _sys :: a,
  _kind :: b,
  _authors :: [c],
  _units :: [d],
  _vars :: [e]
  } -> SystemInformation

-- anything with 'Verb' in it should eventually go
data RefTab where 
  TUnits :: RefTab
  TSymb :: Contents -> RefTab
  --FIXME: Pull out Contents as it's currently a verbatim "intro" to TSymb.
  TVerb :: Section -> RefTab
  -- add more here
data RefSec = RefProg Contents [RefTab] | RefVerb Section -- continue
data DocSection = Verbatim Section | RefSec RefSec

-- Lens (lookup) functions (currently for TSymb)
{-
data LFunc where
  Term :: LFunc
  Defn :: LFunc
  TermExcept :: Concept c => [c] -> LFunc
  DefnExcept :: Concept c => [c] -> LFunc
-}
-- FIXME: Couldn't get this working with TSymb because of the types.
--        We don't necessarily need Concepts for a table of symbols,
--        just Quantity/SymbolForms and this approach didn't allow that.

type DocDesc = [DocSection]

-- 
mkDoc :: DocDesc -> SystemInformation -> Document
mkDoc l si@(SI sys kind authors _ _) = Document 
  ((kind^.term) +:+ S "for" +:+ (sys^.term))
  (manyNames authors) (mkSections si l)

mkSections :: SystemInformation -> DocDesc -> [Section]
mkSections si l = foldr doit [] l
  where
    doit :: DocSection -> [Section] -> [Section]
    doit (Verbatim s) ls = s : ls
    doit (RefSec rs)  ls = mkRefSec si rs : ls

mkRefSec :: SystemInformation -> RefSec -> Section
mkRefSec _  (RefVerb s) = s
mkRefSec si (RefProg c l) = section (refmat^.term) c (foldr (mkSubRef si) [] l)
  where
    mkSubRef :: SystemInformation -> RefTab -> [Section] -> [Section]
    mkSubRef (SI _ _ _ u _)  TUnits   l' = table_of_units u : l'
    mkSubRef (SI _ _ _ _ v) (TSymb con) l' = (mkTSymb v (^.term) con) : l'
    mkSubRef _              (TVerb s) l' = s : l'

mkTSymb :: (Quantity e, SymbolForm e) => 
  [e] -> (e -> Sentence) -> Contents -> Section
mkTSymb v f c = Section (tsymb ^. term) (map Con [c, table v f])
--  where lf Term = (^.term)
--        lf Defn = (^.defn)
--        lf (TermExcept cs) = termExcept cs
--        lf (DefnExcept cs) = defnExcept cs
