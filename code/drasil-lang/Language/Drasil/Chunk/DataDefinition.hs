{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.DataDefinition where

import Language.Drasil.Chunk.Eq (QDefinition)
import Language.Drasil.Spec (Sentence)
import Language.Drasil.Label.Core (Label)
import Language.Drasil.Chunk.References (References)
import Language.Drasil.Chunk.Derivation (Derivation)

import Control.Lens(makeLenses)

-- A data definition is a QDefinition that may have additional notes. 
-- It also has attributes like derivation, source, etc.
data DataDefinition = DD { _qd :: QDefinition
                         , _ref :: References 
                         , _deri :: Derivation 
                         , _lbl :: Label
                         , _notes :: Maybe [Sentence]
                         }
makeLenses ''DataDefinition