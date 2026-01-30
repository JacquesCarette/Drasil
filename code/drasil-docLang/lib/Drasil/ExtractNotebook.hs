-- | Defines functions to extract citation references from Notebook documents.
module Drasil.ExtractNotebook (citeDBLsn) where

import Control.Lens ((^.))
import Data.List (sortBy)
import qualified Data.Set as S

import Drasil.Database (UID)
import Language.Drasil
import Language.Drasil.Development (lnames)
import Drasil.System (System, HasSystem (systemdb))

import Drasil.GetChunks (lookupCitations)
import Drasil.DocumentLanguage.Notebook.Core
import Drasil.ExtractCommon (getCont)

-- | Extracts citation reference 'UID's from a lesson description. This gets all
-- 'UID's that appear in 'Ref' constructors within sentences.
lsnDecCites :: LsnDesc -> [UID]
lsnDecCites = concatMap lsnChapCites

-- | Extracts citation reference 'UID's from a lesson chapter.
lsnChapCites :: LsnChapter -> [UID]
lsnChapCites (Intro (IntrodProg cs)) = concatMap contRefs cs
lsnChapCites (LearnObj (LrnObjProg cs)) = concatMap contRefs cs
lsnChapCites (Review (ReviewProg cs)) = concatMap contRefs cs
lsnChapCites (CaseProb (CaseProbProg cs)) = concatMap contRefs cs
lsnChapCites (Example (ExampleProg cs)) = concatMap contRefs cs
lsnChapCites (Smmry (SmmryProg cs)) = concatMap contRefs cs
lsnChapCites BibSec = []
lsnChapCites (Apndx (ApndxProg cs)) = concatMap contRefs cs

-- | Extracts reference 'UID's from 'Content's.
contRefs :: Contents -> [UID]
contRefs = S.toList . S.unions . map lnames . getCont

-- | Extract bibliography entries for a notebook based on the lesson
-- description. Scans the notebook for citation references and looks them up in
-- the database.
citeDBLsn :: System -> LsnDesc -> BibRef
citeDBLsn si ld = sortBy compareAuthYearTitle refs
  where refs = lookupCitations (si ^. systemdb) (lsnDecCites ld)
