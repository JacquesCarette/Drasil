{-# LANGUAGE Rank2Types #-}
-- Standard code to make a table of symbols.
module Drasil.TableOfAbbAndAcronyms
  ( table_of_abb_and_acronyms ) where

import Control.Lens ((^.))

import Language.Drasil

table_of_abb_and_acronyms :: (Concept s) => 
  [s] -> Section
table_of_abb_and_acronyms ls = Section (S "Abbreviations and Acronyms") 
  [Con (table ls)]
  
--FIXME? Should it be called Symbol or something like Abbreviation/Acronym?
--FIXME? Should it be "Description" or "Term" or something else?
table :: (Concept s) => [s] -> Contents
table ls = Table [S "Symbol", S "Description"] (mkTable
  [(\ch -> ch ^. term) , 
   (\ch -> ch ^. defn)]
  ls)
  (S "Abbreviations and Acronyms") False
