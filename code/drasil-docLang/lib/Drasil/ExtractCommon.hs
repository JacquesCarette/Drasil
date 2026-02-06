module Drasil.ExtractCommon (
  sentToExp, egetCon,
  extractSents, getContList, contRefs
) where

import Control.Lens((^.))
import qualified Data.Set as S

import Drasil.Database (UID)
import Language.Drasil hiding (getCitations, Manual, Verb)
import Language.Drasil.Development (lnames)

-- | Extracts reference 'UID's from 'Content's.
contRefs :: HasContents a => [a] -> S.Set UID
contRefs = S.unions . map lnames . getContList

-- | Extracts all 'ModelExpr's mentioned in a 'Sentence'.
sentToExp :: Sentence -> [ModelExpr]
sentToExp ((:+:) s1 s2) = sentToExp s1 ++ sentToExp s2
sentToExp (E e) = [e]
sentToExp Ch{} = []
sentToExp SyCh{} = []
sentToExp Sy{} = []
sentToExp NP{} = []
sentToExp S{} = []
sentToExp P{} = []
sentToExp (Ref _ s _) = sentToExp s
sentToExp (Quote s) = sentToExp s
sentToExp Percent = []
sentToExp EmptyS = []

-- | Extracts expressions from something that has contents.
egetCon :: HasContents a => a -> [ModelExpr]
egetCon = concatMap sentToExp . extractSents

-- | Extracts 'Sentence's from something that has contents.
extractSents :: HasContents a => a -> [Sentence]
extractSents = go . (^. accessContents)
  where
    -- | Extracts 'Sentence's from raw content.
    go :: RawContent -> [Sentence]
    go (Table s1 s2 t _)   = t : s1 ++ concat s2
    go (Paragraph s)       = [s]
    go (EqnBlock e)        = [eS e]
    go (CodeBlock e)       = [eS' e]
    go (DerivBlock h d)    = h : concatMap go d
    go (Enumeration lst)   = getLT lst
    go (Figure l _ _ _)    = [l]
    go (Bib _)             = []
    go (Graph sss _ _ l)   = let (ls, rs) = unzip sss
                              in l : ls ++ rs
    go (Defini _ ics)      = concatMap (concatMap extractSents . snd) ics

-- | Extracts 'Sentence's from a list of 'Contents'.
getContList :: HasContents a => [a] -> [Sentence]
getContList = concatMap extractSents

-- | Translates different types of lists into a 'Sentence' form.
getLT :: ListType -> [Sentence]
getLT (Bullet it)      = concatMap (getIL . fst) it
getLT (Numeric it)     = concatMap (getIL . fst) it
getLT (Simple lp)      = concatMap getLP lp
getLT (Desc lp)        = concatMap getLP lp
getLT (Definitions lp) = concatMap getLP lp

-- | Translates a 'ListTuple' into 'Sentence's.
getLP :: ListTuple -> [Sentence]
getLP (t, it, _) = t : getIL it

-- | Flattens out an ItemType into 'Sentence's. Headers for 'Nested' items are
-- prepended to its contents.
getIL :: ItemType -> [Sentence]
getIL (Flat s) = [s]
getIL (Nested h lt) = h : getLT lt
