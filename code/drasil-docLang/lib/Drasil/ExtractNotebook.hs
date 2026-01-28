-- | Defines functions to extract citation references from Notebook documents.
module Drasil.ExtractNotebook (getCitations, citeDBLsn) where

import Control.Lens ((^.))

import Drasil.Database (UID)
import Language.Drasil hiding (getCitations, Manual, Verb)
import Language.Drasil.Development (lnames)

import Drasil.DocumentLanguage.Notebook.Core
import Drasil.System (System, HasSystem (systemdb))
import Data.List (sortBy)
import Drasil.GetChunks (lookupCitations)

-- | Extracts citation reference 'UID's from a lesson description.
-- This gets all 'UID's that appear in 'Ref' constructors within sentences.
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
getCitationsCon (UlC (UnlblC rc)) = concatMap lnames (getSentencesRaw rc)
getCitationsCon (LlC lc) = concatMap lnames (getSentencesRaw (lc ^. accessContents))

-- | Extracts 'Sentence's from raw content.
getSentencesRaw :: RawContent -> [Sentence]
getSentencesRaw (Table s1 s2 t _)   = t : s1 ++ concat s2
getSentencesRaw (Paragraph s)       = [s]
getSentencesRaw EqnBlock{}          = []
getSentencesRaw CodeBlock{}         = []
getSentencesRaw (DerivBlock h d)    = h : concatMap getSentencesRaw d
getSentencesRaw (Enumeration lst)   = getLT lst
getSentencesRaw (Figure l _ _ _)    = [l]
getSentencesRaw (Bib _)             = []  -- Don't extract from bibliography itself
getSentencesRaw (Graph [(s1, s2)] _ _ l) = [s1, s2, l]
getSentencesRaw Graph{}             = []
getSentencesRaw (Defini _ [])       = []
getSentencesRaw (Defini dt (hd:fs)) = concatMap getCon' (snd hd) ++ getSentencesRaw (Defini dt fs)

-- | Extracts 'Sentence's from something that has contents.
getCon' :: HasContents a => a -> [Sentence]
getCon' = getSentencesRaw . (^. accessContents)

-- | Translates different types of lists into a 'Sentence' form.
getLT :: ListType -> [Sentence]
getLT (Bullet it) = concatMap (getIL . fst) it
getLT (Numeric it) = concatMap (getIL . fst) it
getLT (Simple lp) = concatMap getLP lp
getLT (Desc lp) = concatMap getLP lp
getLT (Definitions lp) = concatMap getLP lp

-- | Translates a 'ListTuple' into 'Sentence's.
getLP :: ListTuple -> [Sentence]
getLP (t, it, _) = t : getIL it

-- | Flattens out an ItemType into 'Sentence's. Headers for 'Nested' items are
-- prepended to its contents.
getIL :: ItemType -> [Sentence]
getIL (Flat s) = [s]
getIL (Nested h lt) = h : getLT lt

-- | Extract bibliography entries for a notebook based on the lesson description.
-- Scans the notebook for citation references and looks them up in the database.
citeDBLsn :: System -> LsnDesc -> BibRef
citeDBLsn si ld = sortBy compareAuthYearTitle $ lookupCitations (si ^. systemdb) (getCitations ld)
