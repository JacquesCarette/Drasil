module Drasil.ExtractCommon (
  sentToExp, extractMExprs,
  extractSents, extractSents',
  extractChRefs
) where

import Control.Lens((^.))
import qualified Data.Set as S

import Drasil.Database (UID)
import Language.Drasil hiding (getCitations, Manual, Verb)
import Language.Drasil.Development (lnames)

-- | Extracts all referenced 'UID's from things that have 'RawContent's.
extractChRefs :: HasContents a => [a] -> S.Set UID
extractChRefs = S.unions . map lnames . extractSents'

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

-- | Extracts 'ModelExpr's from something that 'HasContents'.
extractMExprs :: HasContents a => a -> [ModelExpr]
extractMExprs = concatMap sentToExp . extractSents

-- | Extracts 'Sentence's from something that 'HasContents'.
extractSents :: HasContents a => a -> [Sentence]
extractSents = go . (^. accessContents)
  where
    -- | Extracts 'Sentence's from 'RawContent'.
    go :: RawContent -> [Sentence]
    go (Table s1 s2 t _)   = t : s1 ++ concat s2
    go (Paragraph s)       = [s]
    go (EqnBlock e)        = [eS e]
    go (CodeBlock e)       = [eS' e]
    go (DerivBlock h d)    = h : concatMap go d
    go (Enumeration lst)   = goList lst
    go (Figure l _ _ _)    = [l]
    go (Bib _)             = []
    go (Graph sss _ _ l)   = let (ls, rs) = unzip sss
                              in l : ls ++ rs
    go (Defini _ ics)      = concatMap (concatMap extractSents . snd) ics
  
    -- | Extracts 'Sentence's from lists.
    goList :: ListType -> [Sentence]
    goList (Bullet it)      = concatMap (goItems . fst) it
    goList (Numeric it)     = concatMap (goItems . fst) it
    goList (Simple lp)      = concatMap goListTitle lp
    goList (Desc lp)        = concatMap goListTitle lp
    goList (Definitions lp) = concatMap goListTitle lp

    -- | Extracts 'Sentence's from list headers.
    goListTitle :: ListTuple -> [Sentence]
    goListTitle (t, it, _) = t : goItems it

    -- | Extract 'Sentence's from 'ItemType's and their nested 'ListType's.
    goItems :: ItemType -> [Sentence]
    goItems (Flat s) = [s]
    goItems (Nested h lt) = h : goList lt

-- | Extracts 'Sentence's from a list of 'Contents'.
extractSents' :: HasContents a => [a] -> [Sentence]
extractSents' = concatMap extractSents
