{-# LANGUAGE GADTs #-}
module Drasil.DocumentLanguage where

import Language.Drasil

import Control.Lens ((^.))

import Drasil.TableOfUnits (table_of_units)

import Data.Drasil.Concepts.Documentation (refmat)

---------------------------------------------------------------------------
-- Start the process of moving away from Document as the main internal
-- representation of information, to something more informative.
-- Over time, we'll want to have a cleaner separation, but doing that
-- all at once would break too much for too long.  So we start here
-- instead.

type System = Sentence
type DocKind = Sentence

data SystemInformation where
 SI :: (Concept a, Concept b, HasName c, Unit d) => {
  _sys :: a,
  _kind :: b,
  _authors :: [c],
  _vars :: [d]
  } -> SystemInformation

-- anything with 'Verb' in it should eventually go
data RefTab = TUnits | TVerb Section -- add more here
data RefSec = RefProg Contents [RefTab] | RefVerb Section -- continue
data DocSection = Verbatim Section | RefSec RefSec

type DocDesc = [DocSection]

-- 
mkDoc :: DocDesc -> SystemInformation -> Document
mkDoc l si@(SI sys kind authors _) = Document 
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
    mkSubRef (SI _ _ _ v) TUnits    l' = table_of_units v : l'
    mkSubRef _            (TVerb s) l' = s : l'
