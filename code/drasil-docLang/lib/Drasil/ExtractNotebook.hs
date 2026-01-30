-- | Defines functions to extract citation references from Notebook documents.
module Drasil.ExtractNotebook (citeDBLsn) where

import Control.Lens ((^.))
import Data.List (sortBy)
import qualified Data.Set as S

import Drasil.Database (UID)
import Language.Drasil hiding (getCitations, Manual, Verb)
import Language.Drasil.Development (lnames)
import Drasil.System (System, HasSystem (systemdb))

import Drasil.GetChunks (lookupCitations)
import Drasil.DocumentLanguage.Notebook.Core
import Drasil.ExtractCommon (getCon)

-- | Extracts citation reference 'UID's from a lesson description. This gets all
-- 'UID's that appear in 'Ref' constructors within sentences.
getCitations :: LsnDesc -> [UID]
getCitations = concatMap getCitationsChap

-- | Extracts citation reference 'UID's from a lesson chapter.
getCitationsChap :: LsnChapter -> [UID]
getCitationsChap (Intro (IntrodProg cs)) = concatMap getCitationsCon cs
getCitationsChap (LearnObj (LrnObjProg cs)) = concatMap getCitationsCon cs
getCitationsChap (Review (ReviewProg cs)) = concatMap getCitationsCon cs
getCitationsChap (CaseProb (CaseProbProg cs)) = concatMap getCitationsCon cs
getCitationsChap (Example (ExampleProg cs)) = concatMap getCitationsCon cs
getCitationsChap (Smmry (SmmryProg cs)) = concatMap getCitationsCon cs
getCitationsChap BibSec = []
getCitationsChap (Apndx (ApndxProg cs)) = concatMap getCitationsCon cs

-- | Extracts citation reference 'UID's from contents.
getCitationsCon :: Contents -> [UID]
getCitationsCon (UlC (UnlblC rc)) = concatMap (S.toList . lnames) (getCon rc)
getCitationsCon (LlC lc) = concatMap (S.toList . lnames) (getCon (lc ^. accessContents))

-- | Extract bibliography entries for a notebook based on the lesson
-- description. Scans the notebook for citation references and looks them up in
-- the database.
citeDBLsn :: System -> LsnDesc -> BibRef
citeDBLsn si ld = sortBy compareAuthYearTitle refs
  where refs = lookupCitations (si ^. systemdb) (getCitations ld)
