{-# LANGUAGE PostfixOperators #-}
-- | Defines functions used to create the Traceability Matrices and Graphs section.
module Drasil.Sections.TraceabilityMandGs (
  -- * Main Functions
  generateTraceTable, traceMatAssumpAssump, traceMatAssumpOther,
  traceMatAssumpOtherUID, traceMatRefinement, traceMatOtherReq, traceMatStandard,
  -- * Helpers
  tvAssumps, tvDataDefns, tvGenDefns, tvTheoryModels,
  tvInsModels, tvGoals, tvReqs, tvChanges, tvLikelyChgs, tvUnlikelyChgs
) where

import Control.Lens((^.))
import Data.Foldable (foldl')

import Drasil.DocumentLanguage.Core (TraceConfig(TraceConfig))
import Drasil.DocumentLanguage.Definitions (TraceViewCat)
import Drasil.DocumentLanguage.TraceabilityMatrix (generateTraceTableView,
  traceMReferrers, traceView, traceViewCC)
import Data.Drasil.Concepts.Documentation (assumption, assumpDom, chgProbDom,
  goalStmt, goalStmtDom, reqDom, item, section_, likelyChg, likeChgDom,
  unlikelyChg, unlikeChgDom)
import Drasil.Metadata.TheoryConcepts (dataDefn, genDefn, inModel, thModel)
import Drasil.Metadata.Documentation (requirement)
import Drasil.Database (mkUid, UID)
import Drasil.System (System, HasSystem(..))
import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators as NC
import qualified Language.Drasil.Development as D
import Language.Drasil.Sentence.Combinators as S

-- | Makes a Traceability Table/Matrix that contains Items of Different Sections.
generateTraceTable :: System -> LabelledContent
generateTraceTable = generateTraceTableView (mkUid "Tracey")
  (titleize' item +:+ S "of Different" +:+ titleize' section_) [tvEverything] [tvEverything]

-- | Traceability viewing everything. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvEverything :: TraceViewCat
tvEverything = flip (const id)

-- | Traceability viewing assumptions. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvAssumps :: TraceViewCat
tvAssumps = traceViewCC assumpDom

-- | Traceability viewing data definitions. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvDataDefns :: TraceViewCat
tvDataDefns = traceView (^. dataDefns)

-- | Traceability viewing general definitions. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvGenDefns :: TraceViewCat
tvGenDefns = traceView (^. genDefns)

-- | Traceability viewing theory models. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvTheoryModels :: TraceViewCat
tvTheoryModels = traceView (^. theoryModels)

-- | Traceability viewing instance models. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvInsModels :: TraceViewCat
tvInsModels = traceView (^. instModels)

-- | Traceability viewing goals. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvGoals :: TraceViewCat
tvGoals = traceViewCC goalStmtDom

-- | Traceability viewing requirements. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvReqs :: TraceViewCat
tvReqs = traceViewCC reqDom

-- | Traceability viewing changes. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvChanges :: TraceViewCat
tvChanges = traceViewCC chgProbDom

-- | Traceability viewing likely changes only. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvLikelyChgs :: TraceViewCat
tvLikelyChgs = traceViewCC likeChgDom

-- | Traceability viewing unlikely changes only. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvUnlikelyChgs :: TraceViewCat
tvUnlikelyChgs = traceViewCC unlikeChgDom

-- | Assumptions on the assumptions of a traceability matrix.
traceMatAssumpAssump :: TraceConfig
traceMatAssumpAssump = TraceConfig (mkUid "TraceMatAvsA") [plural assumption
  +:+ S "on each other"] (titleize' assumption +:+
  S "and Other" +:+ titleize' assumption ) [tvAssumps] [tvAssumps]

-- | UID for the 'traceMatAssumpOther' traceability matrix.
traceMatAssumpOtherUID :: UID
traceMatAssumpOtherUID = mkUid "TraceMatAvsAll"

-- | Other assumptions of the traceability matrix.
-- Conditionally includes likely and unlikely changes in the description
-- based on whether any exist in the system.
traceMatAssumpOther :: System -> TraceConfig
traceMatAssumpOther si = TraceConfig traceMatAssumpOtherUID
  ([plural dataDefn, plural thModel, plural genDefn, plural inModel, plural requirement]
   ++ lcLabel ++ ucLabel)
  (titleize' assumption +:+ S "and Other" +:+ titleize' item) [tvAssumps]
  [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs, tvChanges]
  where
    lcLabel = case traceMReferrers (`tvLikelyChgs` si) si of
      [] -> []
      _  -> [plural likelyChg]
    ucLabel = case traceMReferrers (`tvUnlikelyChgs` si) si of
      [] -> []
      _  -> [D.toSent $ pluralNP (unlikelyChg `NC.onThePP` assumption)]

-- | Refinement of the traceability matrix.
traceMatRefinement :: TraceConfig
traceMatRefinement = TraceConfig (mkUid "TraceMatRefvsRef") [plural dataDefn,
  plural thModel, plural genDefn, plural inModel +:+
  S "on each other"] (titleize' item +:+ S "and Other" +:+ titleize' section_)
  [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels]
  [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels]

-- | Records other requirements. Converts the 'System' into a 'TraceConfig'.
traceMatOtherReq :: System -> TraceConfig
traceMatOtherReq si = TraceConfig (mkUid "TraceMatAllvsR") [plural requirement
  `S.and_` D.toSent (pluralNP (goalStmt `NC.onThePP` dataDefn)), plural thModel,
  plural genDefn, plural inModel] (x titleize' +:+ S "and Other" +:+
  titleize' item) [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs]
  [tvGoals, tvReqs] where
    x g = foldl' (\a (f,t) -> a `sC'` case traceMReferrers (`f` si) si of
      [] -> mempty
      _ -> g t) mempty [(tvReqs, requirement), (tvGoals, goalStmt)]
    sC' EmptyS b = b
    sC' a EmptyS = a
    sC' a b = sC a b

-- | Helpers to check if given argument has more than one peice of information

-- | Contains traceability matrix assumptions, other assumptions, refinement, and other requirements.
traceMatStandard :: System -> [TraceConfig]
traceMatStandard s = map ($ s) [const traceMatAssumpAssump, traceMatAssumpOther, const traceMatRefinement,
  traceMatOtherReq]
