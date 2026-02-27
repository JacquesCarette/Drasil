-- | Defines functions to extract citation references from Notebook documents.
module Drasil.ExtractLsnDesc (extractLsnPlanBib) where

import Control.Lens ((^.))
import qualified Data.Set as S

import Drasil.Database (UID)
import Language.Drasil
import Drasil.System (System, HasSystem (systemdb))

import Drasil.GetChunks (resolveBibliography)
import Drasil.DocumentLanguage.Notebook.Core
import Drasil.ExtractCommon (extractChRefs)

-- | Extracts citation reference 'UID's from a lesson chapter.
lsnChapCites :: LsnChapter -> S.Set UID
lsnChapCites (Intro (IntrodProg cs)) = extractChRefs cs
lsnChapCites (LearnObj (LrnObjProg cs)) = extractChRefs cs
lsnChapCites (Review (ReviewProg cs)) = extractChRefs cs
lsnChapCites (CaseProb (CaseProbProg cs)) = extractChRefs cs
lsnChapCites (Example (ExampleProg cs)) = extractChRefs cs
lsnChapCites (Smmry (SmmryProg cs)) = extractChRefs cs
lsnChapCites BibSec = mempty
lsnChapCites (Apndx (ApndxProg cs)) = extractChRefs cs

-- | Extract bibliography entries for a notebook based on the lesson
-- description. Scans the notebook for citation references and looks them up in
-- the database.
extractLsnPlanBib :: System -> LsnDesc -> BibRef
extractLsnPlanBib si = resolveBibliography (si ^. systemdb) . extractAllRefs
  where
    extractAllRefs = S.unions . map lsnChapCites
