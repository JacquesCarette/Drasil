-- | Re-export database types and functions to simplify external use.
module Database.Drasil (
  -- * Chunk Database
  -- ** Types
  ChunkDB(symbolTable, termTable, conceptChunkTable, _unitTable, _dataDefnTable,
    _insmodelTable, _gendefTable, _theoryModelTable, _conceptinsTable,
    _citationTable, _labelledcontentTable, _traceTable, _refbyTable, _refTable,
    CDB)
  ,  RefbyMap, TraceMap, UMap
  , TermAbbr(..)
  , DomDefn(..)
  -- ** Constructors
  , cdb', idMap, symbolMap, termMap, conceptMap, unitMap, traceMap, generateRefbyMap
  -- ** Lookup Functions
  , asOrderedList, collectUnits, collectAbbreviations
  , termResolve, termResolve', defResolve, symbResolve

  , traceLookup, refbyLookup
  , datadefnLookup, insmodelLookup, gendefLookup, theoryModelLookup
  , conceptinsLookup, refResolve
  -- ** Lenses
  , unitTable, traceTable, refbyTable, citationTable
  , dataDefnTable, insmodelTable, gendefTable, theoryModelTable
  , conceptinsTable, labelledcontentTable, refTable
  -- ** Debugging Tools
  , dumpChunkDB, DumpedChunkDB
  -- ** Helpers
  , addCdb
) where

import Database.Drasil.ChunkDB
import Database.Drasil.Dump
