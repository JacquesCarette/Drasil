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

import Drasil.DocumentLanguage.Core (TraceConfig(TraceConfig))
import Drasil.DocumentLanguage.TraceabilityMatrix (generateTraceTableView,
  traceMReferrers, traceView, traceViewCC, TraceViewCat)

import Data.Drasil.Concepts.Documentation (assumption, assumpDom, chgProbDom,
  goalStmt, goalStmtDom, requirement, reqDom, item, section_, likelyChg,
  unlikelyChg)
import qualified Data.Drasil.TheoryConcepts as Doc (genDefn, dataDefn, inModel, thModel)
import Database.Drasil
import SysInfo.Drasil
import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators as NC
import Language.Drasil.Sentence.Combinators as S
import Data.Foldable (foldl')

-- | Makes a Traceability Table/Matrix that contains Items of Different Sections.
generateTraceTable :: SystemInformation -> LabelledContent
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
tvDataDefns = traceView dataDefnTable

-- | Traceability viewing general definitions. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvGenDefns :: TraceViewCat
tvGenDefns = traceView gendefTable

-- | Traceability viewing theory models. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvTheoryModels :: TraceViewCat
tvTheoryModels = traceView theoryModelTable

-- | Traceability viewing instance models. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvInsModels :: TraceViewCat
tvInsModels = traceView insmodelTable

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
traceMatAssumpOther = TraceConfig (mkUid "TraceMatAvsAll") [plural Doc.dataDefn,
  plural Doc.thModel, plural Doc.genDefn, plural Doc.inModel, plural requirement,
  plural likelyChg, pluralNP (unlikelyChg `NC.onThePP` assumption)]
  (titleize' assumption +:+ S "and Other" +:+ titleize' item) [tvAssumps]
  [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs, tvChanges]

-- | Refinement of the traceability matrix.
traceMatRefinement :: TraceConfig
traceMatRefinement = TraceConfig (mkUid "TraceMatRefvsRef") [plural Doc.dataDefn,
  plural Doc.thModel, plural Doc.genDefn, plural Doc.inModel +:+
  S "on each other"] (titleize' item +:+ S "and Other" +:+ titleize' section_)
  [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels]
  [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels]

-- | Records other requirements. Converts the 'SystemInformation' into a 'TraceConfig'.
traceMatOtherReq :: SystemInformation -> TraceConfig
traceMatOtherReq si = TraceConfig (mkUid "TraceMatAllvsR") [plural requirement
  `S.and_` pluralNP (goalStmt `NC.onThePP` Doc.dataDefn), plural Doc.thModel, 
  plural Doc.genDefn, plural Doc.inModel] (x titleize' +:+ S "and Other" +:+ 
  titleize' item) [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs] 
  [tvGoals, tvReqs] where
    x g = foldl' (\a (f,t) -> a `sC'` case traceMReferrers (flip f $ _sysinfodb si) $
      _sysinfodb si of
      [] -> mempty
      _ -> g t) mempty [(tvReqs, requirement), (tvGoals, goalStmt)]
    sC' EmptyS b = b
    sC' a EmptyS = a
    sC' a b = sC a b

-- | Helpers to check if given argument has more than one peice of information


-- | Contains traceability matrix assumptions, other assumptions, refinement, and other requirements.
traceMatStandard :: SystemInformation -> [TraceConfig]
traceMatStandard s = map ($ s) [const traceMatAssumpAssump, const traceMatAssumpOther, const traceMatRefinement,
  traceMatOtherReq]
