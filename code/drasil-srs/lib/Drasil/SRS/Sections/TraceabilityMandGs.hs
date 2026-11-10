-- | Defines functions used to create the Traceability Matrices and Graphs section.
module Drasil.SRS.Sections.TraceabilityMandGs (
  -- * Main Functions
  traceMatAssumpAssump, traceMatAssumpOther, traceMatRefinement,
  traceMatOtherReq, traceMatStandard,
  -- * Helpers
  tvAssumps, tvDataDefns, tvGenDefns, tvTheoryModels,
  tvInsModels, tvGoals, tvReqs, tvChanges
) where

import Control.Lens ((^.))

-- General Drasil
import Drasil.Database (mkUid)
import Drasil.SRS.SmithEtAlSRS (SmithEtAlSRS, HasSmithEtAlSRS(..))
import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators as NC
import qualified Language.Drasil.Development as D
import Language.Drasil.Sentence.Combinators as S
--
-- Vocabulary
import Drasil.Metadata.TheoryConcepts (dataDefn, genDefn, inModel, thModel)
import Drasil.Metadata.Documentation (assumption, assumpDom, chgProbDom,
  goalStmt, goalStmtDom, reqDom, item, section_, requirement, likelyChg,
  unlikelyChg)

-- Other docLang functions
import Drasil.SRS.DocumentLanguage.Core (TraceConfig(TraceConfig))
import Drasil.SRS.DocumentLanguage.Definitions (TraceViewCat)
import Drasil.SRS.DocumentLanguage.TraceabilityMatrix (traceView, traceViewCC)

-- | Traceability viewing assumptions.
tvAssumps :: TraceViewCat
tvAssumps = traceViewCC assumpDom

-- | Traceability viewing data definitions.
tvDataDefns :: TraceViewCat
tvDataDefns = traceView (^. dataDefns)

-- | Traceability viewing general definitions.
tvGenDefns :: TraceViewCat
tvGenDefns = traceView (^. genDefns)

-- | Traceability viewing theory models.
tvTheoryModels :: TraceViewCat
tvTheoryModels = traceView (^. theoryModels)

-- | Traceability viewing instance models.
tvInsModels :: TraceViewCat
tvInsModels = traceView (^. instModels)

-- | Traceability viewing goals.
tvGoals :: TraceViewCat
tvGoals = traceViewCC goalStmtDom

-- | Traceability viewing requirements.
tvReqs :: TraceViewCat
tvReqs = traceViewCC reqDom

-- | Traceability viewing changes.
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

-- | Chooses the singular or plural noun phrase for a 'NamedIdea' depending on
-- how many instances of it are actually present in the system, instead of
-- assuming there are always several (see #3454).
plOrSg :: NamedIdea n => [a] -> n -> Sentence
plOrSg [_] = phrase
plOrSg _   = plural

-- | Records other requirements. Converts the 'System' into a 'TraceConfig'.
traceMatOtherReq :: SmithEtAlSRS -> TraceConfig
traceMatOtherReq si = TraceConfig (mkUid "TraceMatAllvsR") [plural requirement
  `S.and_` D.toSent (pluralNP (goalStmt `NC.onThePP` dataDefn)),
    plOrSg (si ^. theoryModels) thModel, plOrSg (si ^. genDefns) genDefn,
    plOrSg (si ^. instModels) inModel] (x titleize' +:+ S "and Other" +:+
  titleize' item) [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs]
  [tvGoals, tvReqs] where
    x g = foldl' (\a (f,t) -> a `sC'` case f si of
      [] -> mempty
      _ -> g t) mempty [(tvReqs, requirement), (tvGoals, goalStmt)]
    sC' EmptyS b = b
    sC' a EmptyS = a
    sC' a b = sC a b

-- | Helpers to check if given argument has more than one peice of information

-- | Contains traceability matrix assumptions, other assumptions, refinement, and other requirements.
traceMatStandard :: SmithEtAlSRS -> [TraceConfig]
-- FIXME: This is used in an order-specific fashion...
traceMatStandard s = [traceMatAssumpAssump, traceMatAssumpOther, traceMatRefinement, traceMatOtherReq s]
