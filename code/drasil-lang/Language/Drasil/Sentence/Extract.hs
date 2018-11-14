module Language.Drasil.Sentence.Extract(sdep) where

import Data.List (nub)
import Language.Drasil.UID (UID)
import Language.Drasil.Sentence(Sentence(..), SentenceStyle(..))
import Language.Drasil.Expr.Extract(names)

-- | Generic traverse of all positions that could lead to UIDs from sentences
getUIDs   :: Sentence -> [UID]
getUIDs (Ch SymbolStyle a)   = [a]
getUIDs (Ch _ a)             = []
getUIDs (Sy _)               = []
getUIDs (S _)                = []
getUIDs (Sp _)               = []
getUIDs (P _)                = []
getUIDs (Ref _)              = []
getUIDs ((:+:) a b)          = (getUIDs a) ++ (getUIDs b)
getUIDs (Quote a)            = getUIDs a
getUIDs (E a)                = names a
getUIDs (EmptyS)             = []

-- | Generic traverse of all positions that could lead to UIDs from sentences
getUIDshort   :: Sentence -> [UID]
getUIDshort (Ch ShortStyle a)    = [a]
getUIDshort (Ch _ a)             = []
getUIDshort (Sy _)               = []
getUIDshort (S _)                = []
getUIDshort (Sp _)               = []
getUIDshort (P _)                = []
getUIDshort (Ref _)              = []
getUIDshort ((:+:) a b)          = (getUIDs a) ++ (getUIDs b)
getUIDshort (Quote a)            = getUIDs a
getUIDshort (E a)                = names a
getUIDshort (EmptyS)             = []

-----------------------------------------------------------------------------
-- And now implement the exported traversals all in terms of the above
sdep :: Sentence -> [UID]
sdep = nub . getUIDs
