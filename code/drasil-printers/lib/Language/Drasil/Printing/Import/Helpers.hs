{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- | Printing helpers.
module Language.Drasil.Printing.Import.Helpers (
  -- * Expression-related
  parens,
  -- * Symbol and Term Resolution
  lookupC, lookupC', lookupSymb
) where

import Control.Lens ((^.))

import Drasil.Database (UID, ChunkDB, findOrErr, UIDRef, IsChunk, raw)
import Language.Drasil (Stage(..), codeSymb, eqSymb, Symbol, HasSymbol,
  DefinedQuantityDict)

import qualified Language.Drasil.Printing.AST as P
import Language.Drasil.Printing.PrintingInformation (PrintingInformation, stg, sysdb)

-- * Expr-related

-- | Helper for inserting parentheses.
parens :: P.Expr -> P.Expr
parens = P.Fenced P.Paren P.Paren

-- * Lookup/Term Resolution Functions

-- | Given the stage of the symbol, looks up a character/symbol
-- inside a chunk database that matches the given 'UID'.
lookupC :: Stage -> ChunkDB -> UID -> Symbol
lookupC Equational     sm c = eqSymb   (findOrErr c sm :: DefinedQuantityDict)
lookupC Implementation sm c = codeSymb (findOrErr c sm :: DefinedQuantityDict)

lookupC' :: PrintingInformation -> UID -> Symbol
lookupC' pinfo = lookupC (pinfo ^. stg) (pinfo ^. sysdb)

-- | Look up a symbol given a chunk database and a 'UID' associated with the
-- symbol. Hack: Always uses 'DefinedQuantityDict' as the chunk type to look up,
-- despite that not being the _actual type_ of the chunk being looked up.
--
-- Note: It is because of this function that that the
-- `-Wno-redundant-constraints` OPTIONS_GHC pragma is at the top of the file.
-- This is because we technically don't use `t` at all in the output expression.
-- It can technically be anything!
lookupSymb :: (IsChunk t, HasSymbol t) => PrintingInformation -> UIDRef t -> Symbol
lookupSymb pinfo u = sytyF (pinfo ^. stg) (findOrErr (raw u) (pinfo ^. sysdb) :: DefinedQuantityDict)
  where sytyF Equational = eqSymb
        sytyF Implementation = codeSymb
