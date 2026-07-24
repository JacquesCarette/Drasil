{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- | Printing helpers.
module Language.Drasil.Printing.Import.Helpers (
  -- * Expression-related
  parens,
  -- * Symbol and Term Resolution
  lookupC, lookupC', lookupSymb, lookupT, lookupS, lookupP,
  -- * Capitalization
  resolveCapT, resolveCapP, capHelper
) where

import Control.Lens ((^.))
import Data.Char (toUpper)

import Drasil.Database (UID, ChunkDB, findOrErr, UIDRef, IsChunk, raw)
import Drasil.Database.SearchTools (termResolve', TermAbbr(..))
import Language.Drasil (Stage(..), codeSymb, eqSymb, NounPhrase(..), Sentence(S),
  Symbol, TermCapitalization(..), titleizeNP, titleizeNP', HasSymbol,
  atStartNP, atStartNP', NP, DefinedQuantityDict)
import Language.Drasil.Development (toSent)

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

-- | Look up a term given a chunk database and a 'UID' associated with the term. Also specifies capitalization
lookupT :: PrintingInformation -> UID -> TermCapitalization -> Sentence
lookupT sm c tCap = resolveCapT tCap $ longForm $ termResolve' (sm ^. sysdb) c

-- | Look up the acronym/abbreviation of a term. Otherwise returns the singular form of a term. Takes a chunk database and a 'UID' associated with the term.
lookupS :: PrintingInformation -> UID -> TermCapitalization -> Sentence
lookupS sm c sCap = maybe (resolveCapT sCap $ longForm l) S $ shortForm l >>= capHelper sCap
  where l = termResolve' (sm ^. sysdb) c

-- | Look up the plural form of a term given a chunk database and a 'UID' associated with the term.
lookupP :: PrintingInformation -> UID -> TermCapitalization -> Sentence
lookupP sm c pCap = resolveCapP pCap $ longForm $ termResolve' (sm ^. sysdb) c

-- | Helper to get the proper function for capitalizing a 'NP' based on its 'TermCapitalization'. Singular case.
resolveCapT :: TermCapitalization -> (NP -> Sentence)
resolveCapT NoCap = toSent . phraseNP
resolveCapT CapF = toSent . atStartNP
resolveCapT CapW = toSent . titleizeNP

-- | Helper to get the right function for capitalizing a 'NP' based on its 'TermCapitalization'. Plural case.
resolveCapP :: TermCapitalization -> (NP -> Sentence)
resolveCapP NoCap = toSent . pluralNP
resolveCapP CapF = toSent . atStartNP'
resolveCapP CapW = toSent . titleizeNP'

-- | Helper to get the capital case of an abbreviation based on 'TermCapitalization'. For sentence and title cases.
capHelper :: TermCapitalization -> String -> Maybe String
capHelper NoCap s      = return s
capHelper _     []     = Nothing
capHelper _     (x:xs) = Just (toUpper x: xs)
