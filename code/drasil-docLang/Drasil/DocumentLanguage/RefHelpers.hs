{-# Language Rank2Types #-}
-- | Currently unused. Defines functions to create a database for holding definitions and models.
module Drasil.DocumentLanguage.RefHelpers (
  -- * Type
  ModelDB,
  -- * Functions
  tmRefDB, gdRefDB, ddRefDB,
  imRefDB, mdb, modelsFromDB) where

import Database.Drasil (RefMap, simpleMap)
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)

import Data.List (sortBy)
import Data.Function (on)
import qualified Data.Map as Map (elems)

-- | Get all the models out of a database.
modelsFromDB :: RefMap a -> [a]
modelsFromDB db = dropNums $ sortBy (compare `on` snd) elemPairs
  where elemPairs = Map.elems db
        dropNums = map fst

-- Trying not to add to RefDB since these are recipe-specific content-types for
-- the SmithEtAl Template recipe.
-- | A database that contains 'TheoryModel's, 'GenDefn's, 'DataDefinition's, and 'InstanceModel's.
data ModelDB = MDB
             { tmRefDB :: RefMap TheoryModel
             , gdRefDB :: RefMap GenDefn
             , ddRefDB :: RefMap DataDefinition
             , imRefDB :: RefMap InstanceModel
             }

-- | Constructor for creating a 'ModelDB'.
mdb :: [TheoryModel] -> [GenDefn] -> [DataDefinition] -> [InstanceModel] -> ModelDB
mdb tms gds dds ims = MDB
  (simpleMap tms) (simpleMap gds) (simpleMap dds) (simpleMap ims)
