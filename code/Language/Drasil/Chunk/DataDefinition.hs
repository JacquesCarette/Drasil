{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.DataDefinition where

import Language.Drasil.Chunk.Eq (QDefinition)
import Language.Drasil.Spec (Sentence)

import Control.Lens(makeLenses)

-- A data definition is a QDefinition that may have additional notes. 
-- It also has attributes like derivation, source, etc.
data DataDefinition = DD { _qd :: QDefinition
                         , _notes :: Maybe [Sentence]
                         }
makeLenses ''DataDefinition