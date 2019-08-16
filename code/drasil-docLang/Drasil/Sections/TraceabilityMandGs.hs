module Drasil.Sections.TraceabilityMandGs (generateTraceTable, traceMatAssumpOther,
  traceMatRefinement, traceMatOtherReq, traceMatStandard) where

import Drasil.DocumentLanguage.Core (TraceConfig(TraceConfig))
import Drasil.DocumentLanguage.TraceabilityMatrix (TraceViewCat,
  generateTraceTableView, traceMReferrers, traceView, traceViewCC)

import Data.Drasil.Concepts.Documentation (assumption, assumpDom, chgProbDom,
  goalStmt, goalStmtDom, requirement, reqDom, item, section_, likelyChg,
  unlikelyChg)
import qualified Data.Drasil.IdeaDicts as Doc (genDefn, dataDefn, inModel, thModel)
import Database.Drasil (SystemInformation, _sysinfodb, gendefTable, dataDefnTable,
  insmodelTable, theoryModelTable)
import Language.Drasil

generateTraceTable :: SystemInformation -> LabelledContent
generateTraceTable = generateTraceTableView "Tracey"
  (titleize' item +:+ S "of Different" +:+ titleize' section_) [tvEverything] [tvEverything]

tvEverything :: TraceViewCat
tvEverything = flip (const id)

tvAssumps, tvDataDefns, tvGenDefns, tvTheoryModels, tvInsModels,
  tvGoals, tvReqs, tvChanges :: TraceViewCat
tvAssumps      = traceViewCC assumpDom
tvDataDefns    = traceView   dataDefnTable
tvGenDefns     = traceView   gendefTable
tvTheoryModels = traceView   theoryModelTable
tvInsModels    = traceView   insmodelTable
tvGoals        = traceViewCC goalStmtDom
tvReqs         = traceViewCC reqDom
tvChanges      = traceViewCC chgProbDom

traceMatAssumpOther :: TraceConfig
traceMatAssumpOther = TraceConfig "TraceMatAvsAll" [plural Doc.dataDefn,
  plural Doc.thModel, plural Doc.genDefn, plural Doc.inModel, plural requirement,
  plural likelyChg, plural unlikelyChg +:+ S "on the" +:+. plural assumption]
  (titleize' assumption +:+ S "and Other" +:+ titleize' item) [tvAssumps]
  [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs, tvChanges]

traceMatRefinement :: TraceConfig
traceMatRefinement = TraceConfig "TraceMatRefvsRef" [plural Doc.dataDefn,
  plural Doc.thModel, plural Doc.genDefn, plural Doc.inModel +:+.
  S "with each other"] (titleize' item +:+ S "and Other" +:+ titleize' section_)
  [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels]
  [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels]

traceMatOtherReq :: SystemInformation -> TraceConfig
traceMatOtherReq si = TraceConfig "TraceMatAllvsR" [x plural +:+ S "on the" +:+
  plural Doc.dataDefn, plural Doc.thModel, plural Doc.genDefn, plural Doc.inModel]
  (x titleize' +:+ S "and Other" +:+ titleize' item) [tvDataDefns, tvTheoryModels,
  tvGenDefns, tvInsModels, tvReqs] [tvGoals, tvReqs] where
    x g = foldl (\a (f,t) -> a `sC'` case traceMReferrers (flip f $ _sysinfodb si) $
      _sysinfodb si of
      [] -> mempty
      _ -> g t) mempty [(tvReqs, requirement), (tvGoals, goalStmt)]
    sC' EmptyS b = b
    sC' a EmptyS = a
    sC' a b = sC a b

traceMatStandard :: SystemInformation -> [TraceConfig]
traceMatStandard s = map ($ s) [const traceMatAssumpOther, const traceMatRefinement,
  traceMatOtherReq]
