module Language.Drasil.Sentence.Extract(sdep) where

import Data.List (nub)
import Language.Drasil.UID (UID)
import Language.Drasil.Sentence(Sentence(..))
import Language.Drasil.Expr.Extract(names)

-- | Generic traverse of all positions that could lead to UIDs from sentences
getUIDs   :: Sentence -> [UID]
getUIDs (Ch _ a)       = [a]
getUIDs (Sy _)       = []
getUIDs (S _)        = []
getUIDs (Sp _)       = []
getUIDs (P _)        = []
getUIDs (Ref _)      = []
getUIDs ((:+:) a b)  = (getUIDs a) ++ (getUIDs b)
getUIDs (Quote a)    = getUIDs a
getUIDs (E a)        = names a
getUIDs (EmptyS)     = []

-----------------------------------------------------------------------------
-- And now implement the exported traversals all in terms of the above
sdep :: Sentence -> [UID]
sdep = nub . getUIDs
