{- re-export many things to simplify external use -}
module Language.Drasil.Development (
  -- Development.Unit
    UnitDefn(..)
  , from_udefn, unitCon, makeDerU
  , (^:), (/:), (*:), (*$), (/$),(^$), new_unit
  , scale, shift
  , derUC, derUC', derUC''
  , fund, comp_unitdefn, derCUC, derCUC', derCUC''
  , unitWrapper, getCu, MayHaveUnit(getUnit)
  -- NounPhrase
  , NounPhrase(phraseNP,plural)
  ) where

import Language.Drasil.Development.Unit (UnitDefn(..)
  , from_udefn, unitCon, makeDerU
  , (^:), (/:), (*:), (*$), (/$),(^$), new_unit
  , scale, shift
  , derUC, derUC', derUC''
  , fund, comp_unitdefn, derCUC, derCUC', derCUC''
  , makeDerU, unitWrapper, getCu, MayHaveUnit(getUnit))
import Language.Drasil.NounPhrase (NounPhrase(phraseNP,plural))
