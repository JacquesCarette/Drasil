module Language.Drasil.Printing.Import.Helpers where

import Language.Drasil hiding (neg, sec, symbol, isIn)
import Language.Drasil.Display
import Language.Drasil.Development (UFuncB(..), UFuncVec(..)
  , ArithBinOp(..), BoolBinOp(..), EqBinOp(..), LABinOp(..)
  , OrdBinOp(..), VVNBinOp(..), VVVBinOp(..)
  , precA, precB, eprec, dePrec, dePrecAssoc, DisplayExpr(..)
  , DisplayBinOp(..), DisplayAssocBinOp(Equivalence))
import Database.Drasil
import Utils.Drasil

import qualified Language.Drasil.Printing.AST as P
import qualified Language.Drasil.Printing.Citation as P
import qualified Language.Drasil.Printing.LayoutObj as T
import Language.Drasil.Printing.PrintingInformation (HasPrintingOptions(..),
  PrintingInformation, Notation(Scientific, Engineering), ckdb, stg)

import Control.Lens ((^.))
import Data.Bifunctor (bimap, second)
import Data.List (intersperse, partition)
import Numeric (floatToDigits)
import Data.Maybe (fromMaybe)

-- | Helper for inserting parentheses.
parens :: P.Expr -> P.Expr
parens = P.Fenced P.Paren P.Paren

-- | Processes the digits from the 'floatToDigits' function,
-- decimal point position, a counter, and exponent.
digitsProcess :: [Integer] -> Int -> Int -> Integer -> [P.Expr]
digitsProcess [0] _ _ _ = [P.Int 0, P.MO P.Point, P.Int 0]
digitsProcess ds pos _ (-3) = [P.Int 0, P.MO P.Point] ++ replicate (3 - pos) (P.Int 0) ++ map P.Int ds
digitsProcess (hd:tl) pos coun ex
  | pos /= coun = P.Int hd : digitsProcess tl pos (coun + 1) ex
  | ex /= 0 = [P.MO P.Point, P.Int hd] ++ map P.Int tl ++ [P.MO P.Dot, P.Int 10, P.Sup $ P.Int ex]
  | otherwise = [P.MO P.Point, P.Int hd] ++ map P.Int tl
digitsProcess [] pos coun ex
  | pos > coun = P.Int 0 : digitsProcess [] pos (coun+1) ex
  | ex /= 0 = [P.MO P.Point, P.Int 0, P.MO P.Dot, P.Int 10, P.Sup $ P.Int ex]
  | otherwise = [P.MO P.Point, P.Int 0]

-- | Takes the exponent and the 'Int' of the base and gives
-- the decimal point position and processed exponent.
-- This function supports transferring scientific notation to
-- engineering notation.
-- References for standard of Engineering Notation:
--
-- https://www.khanacademy.org/science/electrical-engineering/introduction-to-ee/
--    intro-to-ee/a/ee-numbers-in-electrical-engineering 
--
-- https://www.calculatorsoup.com/calculators/math/scientific-notation-converter.php
--
-- https://en.wikipedia.org/wiki/Scientific_notation
processExpo :: Int -> (Int, Int)
processExpo a
  | mod (a-1) 3 == 0 = (1, a-1)
  | mod (a-1) 3 == 1 = (2, a-2)
  | mod (a-1) 3 == 2 = (3, a-3)
  | otherwise = error "The cases of processExpo should be exhaustive!"

-- | Given the stage of the symbol, looks up a character/symbol
-- inside a chunk database that matches the given 'UID'. 
lookupC :: Stage -> ChunkDB -> UID -> Symbol
lookupC Equational     sm c = eqSymb   $ symbResolve sm c
lookupC Implementation sm c = codeSymb $ symbResolve sm c

-- | Look up a term given a chunk database and a 'UID' associated with the term.
lookupT :: ChunkDB -> UID -> Sentence
lookupT sm c = phraseNP $ termResolve sm c ^. term

-- | Look up the acronym/abbreviation of a term. Otherwise returns the singular form of a term. Takes a chunk database and a 'UID' associated with the term.
lookupS :: ChunkDB -> UID -> Sentence
lookupS sm c = maybe (phraseNP $ l ^. term) S $ getA l
  where l = termResolve sm c

-- | Look up the plural form of a term given a chunk database and a 'UID' associated with the term.
lookupP :: ChunkDB -> UID -> Sentence
lookupP sm c = pluralNP $ termResolve sm c ^. term
