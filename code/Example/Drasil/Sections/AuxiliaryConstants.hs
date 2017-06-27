module Drasil.Sections.AuxiliaryConstants 
  (valsOfAuxConstantsF) where

import Language.Drasil
import qualified Drasil.SRS as SRS
import Data.Drasil.SentenceStructures (foldlSP)
import Data.Drasil.Concepts.Documentation

valsOfAuxConstantsF :: [QDefinition] -> Section
valsOfAuxConstantsF listOfConstants = (SRS.valsOfAuxCons) [intro, tableOfConstants (listOfConstants)] []

--FIXME: general introduction?
intro :: Contents
intro = foldlSP [S "Insert introduction here"]

tableOfConstants :: [QDefinition] -> Contents
tableOfConstants f = Table
  [S "Constants"]
  (mkTable [(\c -> E $ equat c)] f)
  (S "Table of Auxiliary Constants")
  True

{-
s6_2_5_table3 = Table [S "Var", S "Physical Constraints"] 
  (mkTable [(\x -> P $ fst(x)), (\x -> snd(x))] [(prob_br ^. symbol, E (Int 0 :< C prob_br :< Int 1))])
  (titleize table_ +: S "4" +:+ titleize output_ +:+ titleize' variable) True
-}