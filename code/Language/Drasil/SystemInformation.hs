{-# LANGUAGE GADTs #-}

module Language.Drasil.SystemInformation where

import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.Constrained
import Language.Drasil.ChunkDB (ChunkDB)
import Language.Drasil.People
import Language.Drasil.Reference
import Language.Drasil.Unit

-- | Data structure for holding all of the requisite information about a system
-- to be used in artefact generation
data SystemInformation where
--FIXME:
--There should be a way to remove redundant "Quantity" constraint.
-- I'm thinking for getting concepts that are also quantities, we could
-- use a lookup of some sort from their internal (Drasil) ids.
-- FIXME: b shouldn't need to be a NounPhrase, this will be fixed after
-- NP is built into NamedIdea.
 SI :: (NamedIdea a, NamedIdea b, HasName c, Unit d,
  Quantity e, Ord e, Ord f, Quantity f, Concept f,
  Quantity h, Quantity i,
  Constrained j) => 
  { _sys :: a
  , _kind :: b
  , _authors :: [c]
  , _units :: [d]
  , _quants :: [e]
  , _concepts :: [f]
  , _definitions :: [QDefinition]
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
