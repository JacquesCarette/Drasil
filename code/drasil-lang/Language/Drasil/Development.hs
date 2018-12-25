{- re-export many things to simplify external use -}
module Language.Drasil.Development (
  -- Development.Unit
    UnitDefn(..)
  , from_udefn, unitCon, makeDerU
  , (^:), (/:), (*:), (*$), (/$),(^$), new_unit
  , scale, shift
  , derUC, derUC', derUC''
  , fund, fund', comp_unitdefn, derCUC, derCUC', derCUC''
  , unitWrapper, getCu, MayHaveUnit(getUnit)
  -- NounPhrase
  , NounPhrase(phraseNP,pluralNP)
  -- Expr.Extract
  , dep, names, names'
  -- Sentence.Extract
  , sdep, lnames, lnames'
  -- Expr.Precendence
  , precA, precB, eprec
  ) where

import Language.Drasil.Development.Unit (UnitDefn(..)
  , from_udefn, unitCon, makeDerU
  , (^:), (/:), (*:), (*$), (/$),(^$), new_unit
  , scale, shift
  , derUC, derUC', derUC''
  , fund, fund', comp_unitdefn, derCUC, derCUC', derCUC''
  , makeDerU, unitWrapper, getCu, MayHaveUnit(getUnit))
import Language.Drasil.NounPhrase (NounPhrase(phraseNP,pluralNP))
import Language.Drasil.Expr.Extract (dep, names', names)
import Language.Drasil.Expr.Precedence (precA, precB, eprec)
import Language.Drasil.Sentence.Extract(sdep, lnames, lnames')
