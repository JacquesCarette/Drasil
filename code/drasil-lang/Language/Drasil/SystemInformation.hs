{-# LANGUAGE GADTs #-}

module Language.Drasil.SystemInformation where

import Language.Drasil.Chunk.Citation (BibRef)
import Language.Drasil.Chunk.Eq (QDefinition)
import Language.Drasil.Chunk.Quantity (Quantity)
import Language.Drasil.ChunkDB (ChunkDB)
import Language.Drasil.Classes (CommonIdea, Concept, Constrained, HasUID, 
  Idea, IsUnit)
import Language.Drasil.People (HasName)
import Language.Drasil.Reference (ReferenceDB, citationsFromBibMap, 
  citationRefTable)

import Control.Lens ((^.))

import Language.Drasil.Chunk.DataDefinition (DataDefinition)

-- | Data structure for holding all of the requisite information about a system
-- to be used in artefact generation
data SystemInformation where
--FIXME:
--There should be a way to remove redundant "Quantity" constraint.
-- I'm thinking for getting concepts that are also quantities, we could
-- use a lookup of some sort from their internal (Drasil) ids.
 SI :: (CommonIdea a, Idea a, Idea b, HasName c, IsUnit d,
  Quantity e, Eq e, Quantity f, Concept f, Eq f,
  Quantity h, Quantity i,
  HasUID j, Constrained j) => 
  { _sys :: a
  , _kind :: b
  , _authors :: [c]
  , _units :: [d]
  , _quants :: [e]
  , _concepts :: [f]
  , _definitions :: [QDefinition] --FIXME: will be removed upon migration to use of [DataDefinition] below
  , _datadefs :: [DataDefinition]
  , _inputs :: [h]
  , _outputs :: [i]
  , _defSequence :: [Block QDefinition]
  , _constraints :: [j] --TODO: Add SymbolMap OR enough info to gen SymbolMap
  , _constants :: [QDefinition]
  , _sysinfodb :: ChunkDB
  , _refdb :: ReferenceDB
  } -> SystemInformation
  
-- | for listing QDefs in SystemInformation
data Block a = Coupled a a [a]
           | Parallel a [a]

-- | Helper for extracting bibliography
citeDB :: SystemInformation -> BibRef
citeDB (SI {_refdb = db}) = citationsFromBibMap (db ^. citationRefTable)


-- | Helper for ectracting RefDB
getRefDB :: SystemInformation -> ReferenceDB
getRefDB (SI {_refdb = db}) = db