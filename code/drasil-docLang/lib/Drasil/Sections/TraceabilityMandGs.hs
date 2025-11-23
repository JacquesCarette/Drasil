{-# LANGUAGE PostfixOperators #-}
-- | Defines functions used to create the Traceability Matrices and Graphs section.
module Drasil.Sections.TraceabilityMandGs (
  -- * Main Functions
  generateTraceTable, traceMatAssumpAssump, traceMatAssumpOther,
  traceMatRefinement, traceMatOtherReq, traceMatStandard,
  -- * Helpers
  tvAssumps, tvDataDefns, tvGenDefns, tvTheoryModels,
  tvInsModels, tvGoals, tvReqs, tvChanges
) where

import Control.Lens((^.))
import Data.Foldable (foldl')

import Drasil.DocumentLanguage.Core (TraceConfig(TraceConfig))
import Drasil.DocumentLanguage.TraceabilityMatrix (generateTraceTableView,
  traceMReferrers, traceView, traceViewCC, TraceViewCat)
import Data.Drasil.Concepts.Documentation (assumption, assumpDom, chgProbDom,
  goalStmt, goalStmtDom, requirement, reqDom, item, section_, likelyChg,
  unlikelyChg)
import Drasil.Metadata (dataDefn, genDefn, inModel, thModel)
import Drasil.Database
import Drasil.Database.SearchTools
import Drasil.System
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
tvDataDefns = traceView (findAllDataDefns . (^. systemdb))

-- | Traceability viewing general definitions. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvGenDefns :: TraceViewCat
tvGenDefns = traceView (findAllGenDefns . (^. systemdb))

-- | Traceability viewing theory models. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvTheoryModels :: TraceViewCat
tvTheoryModels = traceView (findAllTheoryMods . (^. systemdb))

-- | Traceability viewing instance models. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvInsModels :: TraceViewCat
tvInsModels = traceView (findAllInstMods . (^. systemdb))

-- | Traceability viewing goals. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvGoals :: TraceViewCat
tvGoals = traceViewCC goalStmtDom

-- | Traceability viewing requirements. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvReqs :: TraceViewCat
tvReqs = traceViewCC reqDom

-- | Traceability viewing changes. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvChanges :: TraceViewCat
tvChanges = traceViewCC chgProbDom

-- | Assumptions on the assumptions of a traceability matrix.
traceMatAssumpAssump :: TraceConfig
traceMatAssumpAssump = TraceConfig (mkUid "TraceMatAvsA") [plural assumption
  +:+ S "on each other"] (titleize' assumption +:+
  S "and Other" +:+ titleize' assumption ) [tvAssumps] [tvAssumps]

-- | Other assumptions of the traceability matrix
traceMatAssumpOther :: TraceConfig
traceMatAssumpOther = TraceConfig (mkUid "TraceMatAvsAll") [plural dataDefn,
  plural thModel, plural genDefn, plural inModel, plural requirement,
  plural likelyChg, D.toSent $ pluralNP (unlikelyChg `NC.onThePP` assumption)]
  (titleize' assumption +:+ S "and Other" +:+ titleize' item) [tvAssumps]
  [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs, tvChanges]

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
traceMatStandard s = map ($ s) [const traceMatAssumpAssump, const traceMatAssumpOther, const traceMatRefinement,
  traceMatOtherReq]
