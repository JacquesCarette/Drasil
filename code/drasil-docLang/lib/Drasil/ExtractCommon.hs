module Drasil.ExtractCommon (
  sentToExp, extractMExprs,
  extractSents, extractSents',
  extractChRefs,
  getSec,
  extractSectionsBib,
  resolveBibliography
) where

import Control.Lens ((^.))
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S

import Drasil.Database (UID, ChunkDB, find)
import Language.Drasil hiding (getCitations, Manual, Verb)
import Language.Drasil.Document (HasContents(..), RawContent(..), ListType(..),
  ItemType(..), ListTuple, Section (..), SecCons (..))
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
    go (Defini ics)        = concatMap (concatMap extractSents . snd) ics

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

-- | Extracts 'Sentence's from a 'Section'.
getSec :: Section -> [Sentence]
getSec (Section t sc _ ) = t : concatMap getSecCon sc

-- | Extracts 'Sentence's from section contents.
getSecCon :: SecCons -> [Sentence]
getSecCon (Sub s) = getSec s
getSecCon (Con c) = extractSents c

-- | Extract bibliography entries from generated sections. This version extracts
-- from fully expanded Sections, capturing citations that are only created
-- during document generation (like those in orgOfDocIntro).
extractSectionsBib :: ChunkDB -> [Section] -> BibRef
extractSectionsBib db = resolveBibliography db . extractAllSecRefs
  where
    extractAllSecRefs = S.unions . map (S.unions . map lnames . getSec)

-- | Given a 'ChunkDB' and a set of 'UID's, looks up the corresponding
-- 'Citation's and returns them sorted by author, year, and title.
--
-- FIXME: This function assumes that all 'UID's in the set correspond to
-- 'Citation's in the database. If a 'UID' does not correspond to a 'Citation',
-- it is simply ignored. This should rather rely on a set of 'UIDRef Citation's.
resolveBibliography :: ChunkDB -> S.Set UID -> [Citation]
resolveBibliography db uids = sortBy compareAuthYearTitle cites
  where
    cites = mapMaybe (`find` db) (S.toList uids)
