module Language.Drasil.Sentence.Extract(sdep, shortdep, lnames, lnames') where

import Data.List (nub)
import Language.Drasil.UID (UID)
import Language.Drasil.Sentence(Sentence(..), SentenceStyle(..))
import Language.Drasil.RefProg (Reference2(Reference2))
import Language.Drasil.Expr.Extract(names)


-- | Generic traverse of all positions that could lead to UIDs from sentences
getUIDs   :: Sentence -> [UID]
getUIDs (Ch SymbolStyle a)   = [a]
getUIDs (Ch ShortStyle _)    = []
getUIDs (Ch TermStyle _)     = []
getUIDs (Ch PluralTerm _)    = []
getUIDs (Sy _)               = []
getUIDs (S _)                = []
getUIDs (Sp _)               = []
getUIDs (P _)                = []
getUIDs (Ref _)              = []
getUIDs (Ref2 _)             = []
getUIDs ((:+:) a b)          = (getUIDs a) ++ (getUIDs b)
getUIDs (Quote a)            = getUIDs a
getUIDs (E a)                = names a
getUIDs (EmptyS)             = []

-- | Generic traverse of all positions that could lead to UIDs from sentences
getUIDshort   :: Sentence -> [UID]
getUIDshort (Ch ShortStyle a)    = [a]
getUIDshort (Ch SymbolStyle _)   = []
getUIDshort (Ch TermStyle _)     = []
getUIDshort (Ch PluralTerm _)    = []
getUIDshort (Sy _)               = []
getUIDshort (S _)                = []
getUIDshort (Sp _)               = []
getUIDshort (P _)                = []
getUIDshort (Ref _)              = []
getUIDshort (Ref2 _)             = []
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
lnames  (Sp _)         = []
lnames  (P _)          = []
lnames  (Ref _)  = []
lnames  (Ref2 (Reference2 u _ _ _)) = [u] -- This should be fixed.
lnames  ((:+:) a b)    = (lnames  a) ++ (lnames  b)
lnames  (Quote a)      = []
lnames  (E a)          = []
lnames  (EmptyS)       = []

lnames'  :: [Sentence] -> [UID]
lnames'  (hd:tl) = lnames  hd ++ lnames'  tl
lnames'  (hd:[]) = lnames  hd
lnames'  []      = []