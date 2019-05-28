module Language.Drasil.Sentence.Extract(sdep, shortdep, lnames, lnames') where

import Data.List (nub)
import Language.Drasil.UID (UID)
import Language.Drasil.Sentence(Sentence(..), SentenceStyle(..))
import Language.Drasil.RefProg (Reference(Reference))
import Language.Drasil.Expr.Extract(names)


-- | Generic traverse of all positions that could lead to UIDs from sentences
getUIDs   :: Sentence -> [UID]
getUIDs (Ch SymbolStyle a)   = [a]
getUIDs (Ch ShortStyle _)    = []
getUIDs (Ch TermStyle _)     = []
getUIDs (Ch PluralTerm _)    = []
getUIDs (Sy _)               = []
getUIDs (S _)                = []
getUIDs (P _)                = []
getUIDs (Ref _)              = []
getUIDs Percent              = []
getUIDs ((:+:) a b)          = (getUIDs a) ++ (getUIDs b)
getUIDs (Quote a)            = getUIDs a
getUIDs (E a)                = names a
getUIDs (EmptyS)             = []

-- | Generic traverse of all positions that could lead to UIDs from sentences
-- but don't go into expressions.
getUIDshort   :: Sentence -> [UID]
getUIDshort (Ch ShortStyle a)    = [a]
getUIDshort (Ch SymbolStyle _)   = []
getUIDshort (Ch TermStyle _)     = []
getUIDshort (Ch PluralTerm _)    = []
getUIDshort (Sy _)               = []
getUIDshort (S _)                = []
getUIDshort Percent              = []
getUIDshort (P _)                = []
getUIDshort (Ref _)              = []
getUIDshort ((:+:) a b)          = (getUIDshort a) ++ (getUIDshort b)
getUIDshort (Quote a)            = getUIDshort a
getUIDshort (E _)                = []
getUIDshort (EmptyS)             = []

-----------------------------------------------------------------------------
-- And now implement the exported traversals all in terms of the above
-- This is to collect UID who is printed out as a Symbol
sdep :: Sentence -> [UID]
sdep = nub . getUIDs

-- This is to collect UID who is printed out as an Abbreviation
shortdep :: Sentence -> [UID]
shortdep = nub . getUIDshort

-- | Generic traverse of all positions that could lead to reference UID from sentences
lnames   :: Sentence -> [UID]
lnames  (Ch _ _)       = []
lnames  (Sy _)         = []
lnames  (S _)          = []
lnames  Percent        = []
lnames  (P _)          = []
lnames  (Ref (Reference u _ _ _)) = [u] -- This should be fixed.
lnames  ((:+:) a b)    = (lnames  a) ++ (lnames  b)
lnames  (Quote _)      = []
lnames  (E _)          = []
lnames  (EmptyS)       = []

lnames'  :: [Sentence] -> [UID]
lnames' = concatMap lnames 
