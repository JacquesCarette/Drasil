-- | Defines functions to extract citation references from Notebook documents.
module Drasil.ExtractLsnDesc (lsnPlanCites) where

import Control.Lens ((^.))
import qualified Data.Set as S

import Drasil.Database (UID)
import Language.Drasil
import Language.Drasil.Development (lnames)
import Drasil.System (System, HasSystem (systemdb))

import Drasil.GetChunks (resolveBibliography)
import Drasil.DocumentLanguage.Notebook.Core
import Drasil.ExtractCommon (getContList)

-- | Extracts citation reference 'UID's from a lesson description. This gets all
-- 'UID's that appear in 'Ref' constructors within sentences.
lsnDecCites :: LsnDesc -> S.Set UID
lsnDecCites d = S.unions $ map lsnChapCites d

-- | Extracts citation reference 'UID's from a lesson chapter.
lsnChapCites :: LsnChapter -> S.Set UID
lsnChapCites (Intro (IntrodProg cs)) = contRefs cs
lsnChapCites (LearnObj (LrnObjProg cs)) = contRefs cs
lsnChapCites (Review (ReviewProg cs)) = contRefs cs
lsnChapCites (CaseProb (CaseProbProg cs)) = contRefs cs
lsnChapCites (Example (ExampleProg cs)) = contRefs cs
lsnChapCites (Smmry (SmmryProg cs)) = contRefs cs
lsnChapCites BibSec = mempty
lsnChapCites (Apndx (ApndxProg cs)) = contRefs cs

-- | Extracts reference 'UID's from 'Content's.
contRefs :: [Contents] -> S.Set UID
contRefs = S.unions . map lnames . getContList

-- | Extract bibliography entries for a notebook based on the lesson
-- description. Scans the notebook for citation references and looks them up in
-- the database.
lsnPlanCites :: System -> LsnDesc -> BibRef
lsnPlanCites si = resolveBibliography (si ^. systemdb) . lsnDecCites
