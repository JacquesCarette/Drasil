{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.DataDefinition where

import Language.Drasil.Chunk.Eq (QDefinition)
import Language.Drasil.Spec (Sentence(EmptyS))
import Language.Drasil.Label.Core (Label)
import Language.Drasil.Chunk.References (References)
import Language.Drasil.Chunk.Derivation (Derivation)
import Language.Drasil.Expr (Expr)
import Language.Drasil.Chunk.Quantity (Quantity(getUnit), HasSpace(typ), QuantityDict,
  mkQuant, qw)
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term))
import Language.Drasil.Chunk.SymbolForm (eqSymb)

import Control.Lens(makeLenses, (^.))

import Language.Drasil.Chunk.Eq(fromEqn, fromEqn')

-- A data definition is a QDefinition that may have additional notes. 
-- It also has attributes like derivation, source, etc.
data DataDefinition = DD { _qd :: QDefinition
                         , _ref :: References 
                         , _deri :: Derivation 
                         , _lbl :: Label
                         , _notes :: Maybe [Sentence]
                         }
makeLenses ''DataDefinition

-- Used to help make Qdefinitions when uid, term, and symbol come from the same source
mkDataDef :: (Quantity c) => c -> Expr -> QDefinition
mkDataDef cncpt equation = datadef $ getUnit cncpt --should references be passed in at this point?
  where datadef (Just a) = fromEqn  (cncpt ^. uid) (cncpt ^. term) EmptyS
                           (eqSymb cncpt) a equation [] (cncpt ^. uid) --shortname
        datadef Nothing  = fromEqn' (cncpt ^. uid) (cncpt ^. term) EmptyS
                           (eqSymb cncpt) equation [] (cncpt ^. uid) --shortname

-- FIXME: should be removed soon
-- Same as 'mkDataDef', but with an additional Sentence that can be taken as "extra information"; issue #350
mkDataDef' :: (Quantity c) => c -> Expr -> Sentence -> References -> QDefinition
mkDataDef' cncpt equation extraInfo refs = datadef $ getUnit cncpt
  where datadef (Just a) = fromEqn  (cncpt ^. uid) (cncpt ^. term) extraInfo
                           (eqSymb cncpt) a equation refs (cncpt ^. uid) --shortname
        datadef Nothing  = fromEqn' (cncpt ^. uid) (cncpt ^. term) extraInfo
                           (eqSymb cncpt) equation refs (cncpt ^. uid) --shortname