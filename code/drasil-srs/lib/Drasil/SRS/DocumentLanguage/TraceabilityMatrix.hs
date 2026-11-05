-- | Defines functions to create traceability matrices in SRS documents.
module Drasil.SRS.DocumentLanguage.TraceabilityMatrix (
  generateTraceTableView, layoutUIDs, traceMColumns, traceMIntro, traceView,
  traceViewCC
) where

-- General Haskell
import Control.Lens ((^.))
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

-- General Drasil
import Drasil.Database (UID, HasUID(..), dependencies)
import Drasil.Database.SearchTools (defResolve', findAllConcInsts, DomDefn(domain))
import Language.Drasil
import Language.Drasil.Document
import Drasil.System (systemdb)
import Drasil.SRS.SmithEtAlSRS (SmithEtAlSRS)
import qualified Language.Drasil.Sentence.Combinators as S

-- Vocabulary
import Drasil.Metadata.Documentation (purpose, component, dependency,
  item, reference, traceyMatrix)

-- Other docLang
import Drasil.SRS.DocumentLanguage.Definitions (helpToRefField, TraceViewCat)

-- FIXME: Everything in this file needs to be re-written for readability.

-- * Main Functions

-- | Generalized traceability matrix introduction: appends references to the traceability matrices in 'Sentence' form
-- and wraps in 'Contents'. Usually references the four tables generally found in this section (in order of being mentioned).
traceMIntro :: [LabelledContent] -> [Sentence] -> Contents
traceMIntro refs trailings = UlC $ ulcc $ Para [foldlSent [phrase purpose
        `S.the_ofTheC` plural traceyMatrix, S "is to provide easy", plural reference,
        S "on what has to be additionally modified if a certain",
        phrase component, S "is changed. Every time a", phrase component,
        S "is changed, the", plural item, S "in the column of that",
        phrase component, S "that are marked with an", Quote (S "X"),
        S "should be modified as well"], foldlSent_ (zipWith tableShows refs trailings)]

-- | Generates a traceability table. Takes a 'UID' for the table, a description ('Sentence'), columns ('TraceViewCat'), rows ('TraceViewCat'), and 'System'.
generateTraceTableView :: UID -> Sentence -> [TraceViewCat] -> [TraceViewCat] -> SmithEtAlSRS -> LabelledContent
generateTraceTableView u desc cols rows c = llccTab' u $ Table
  (EmptyS : traceMHeader colUIDs c)
  (makeTMatrix (traceMHeader rowUIDs c) (traceMColumns colUIDs rowUIDs c) colUIDs)
  (showingCxnBw traceyMatrix desc) True
    where
    colUIDs = layoutUIDs cols c
    rowUIDs = layoutUIDs rows c

-- * Helper Functions

-- | Helper that finds the header of a traceability matrix.
traceMHeader :: [UID] -> SmithEtAlSRS -> [Sentence]
traceMHeader us c = (`helpToRefField` (c ^. systemdb)) <$> us

-- | Helper that makes the columns of a traceability matrix: for each row, the
-- column 'UID's (in column order) that the row references.
traceMColumns :: [UID] -> [UID] -> SmithEtAlSRS -> [[UID]]
traceMColumns cols rows c = (\refs -> filter (`S.member` refs) cols) . rowRefs <$> rows
  where rowRefs r = fromMaybe S.empty $ dependencies r (c ^. systemdb)

-- | Helper that makes references of the form "@reference@ shows the dependencies of @something@".
tableShows :: (Referable a, HasShortName a) => a -> Sentence -> Sentence
tableShows r end = refS r +:+ S "shows the" +:+ plural dependency `S.ofThe` (end !.)

-- | Helper that finds the layout 'UID's of a traceability matrix.
layoutUIDs :: [TraceViewCat] -> SmithEtAlSRS -> [UID]
layoutUIDs cats c = concatMap ($ c) cats

-- | Helper that filters a traceability matrix given a predicate and a 'ChunkDB' lens field.
traceViewFilt :: HasUID a => (a -> Bool) -> (SmithEtAlSRS -> [a]) -> TraceViewCat
traceViewFilt f table = fmap (^. uid) . filter f . table

-- | Helper that is similar to 'traceViewFilt', but the filter is always 'True'.
traceView :: HasUID a => (SmithEtAlSRS -> [a]) -> TraceViewCat
traceView = traceViewFilt (const True)

-- | Turns a 'Concept' into a 'TraceViewCat' via its domain.
traceViewCC :: Concept c => c -> TraceViewCat
traceViewCC dom s = traceViewFilt (isDomUnder (dom ^. uid) . sDom . cdom) (findAllConcInsts . (^. systemdb)) s
  where
    isDomUnder :: UID -> UID -> Bool
    isDomUnder filtDom curr
      | filtDom == curr = True
      | not $ null $ getDom curr = isDomUnder filtDom (sDom $ getDom curr)
      | otherwise = False
    getDom :: UID -> [UID]
    getDom curr = domain $ defResolve' (s ^. systemdb) curr
