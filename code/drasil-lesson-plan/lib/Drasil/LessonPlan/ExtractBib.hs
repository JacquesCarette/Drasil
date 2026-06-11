-- | Defines functions to extract citation references from Notebook documents.
module Drasil.LessonPlan.ExtractBib (extractBib) where

import qualified Data.Set as S

import Drasil.Database (UID, ChunkDB)
import Language.Drasil
import Language.Drasil.Document

import Drasil.LessonPlan.Document
import Language.Drasil.Development (lnames)

findAllInConsSecs :: [Contents] -> [Section] -> S.Set UID
findAllInConsSecs cs ss = S.unions $
  extractChRefs cs : concatMap (map lnames . getSec) ss

-- | Extracts citation reference 'UID's from a lesson chapter.
lsnChapCites :: LsnChapter -> S.Set UID
lsnChapCites (Intro cs) = extractChRefs cs
lsnChapCites (LearnObj cs) = extractChRefs cs
lsnChapCites (Review cs ss) = findAllInConsSecs cs ss
lsnChapCites (CaseProb cs ss) = findAllInConsSecs cs ss
lsnChapCites (Example cs) = extractChRefs cs
lsnChapCites (Smmry cs) = extractChRefs cs
lsnChapCites BibSec = mempty
lsnChapCites (Apndx cs) = extractChRefs cs

-- | Extract bibliography entries for a notebook based on the lesson
-- description. Scans the notebook for citation references and looks them up in
-- the database.
extractBib :: ChunkDB -> LsnDesc -> BibRef
extractBib db = resolveBibliography db . extractAllRefs
  where
    extractAllRefs = S.unions . map lsnChapCites
