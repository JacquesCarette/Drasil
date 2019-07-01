module Drasil.Sections.TraceabilityMandGs (generateTraceTable,tvAssumps,
  tvDataDefns, tvGenDefns, tvTheoryModels, tvInsModels, tvGoals, tvReqs,
  tvChanges, traceMatAssumpOther, traceMatRefinement, traceMatOtherReq,
  traceMatStandard) where

import Drasil.DocumentLanguage.TraceabilityMatrix (generateTraceTableView,
  traceMReferrers, traceView, traceViewCC, TraceViewCat)

import Data.Drasil.Concepts.Documentation (assumption, assumpDom, chgProbDom,
  goalStmt, goalStmtDom, requirement, reqDom, item, section_, likelyChg,
  unlikelyChg)
import qualified Data.Drasil.IdeaDicts as Doc (genDefn, dataDefn, inModel, thModel)
import Database.Drasil(SystemInformation, _sysinfodb, gendefTable, dataDefnTable,
  insmodelTable, theoryModelTable)
import Language.Drasil

generateTraceTable :: SystemInformation -> LabelledContent
generateTraceTable = generateTraceTableView "Tracey"
  (titleize' item +:+ S "of Different" +:+ titleize' section_) [tvEverything] [tvEverything]

tvEverything :: TraceViewCat
tvEverything = flip (const id)

tvAssumps :: TraceViewCat
tvAssumps = traceViewCC assumpDom

tvDataDefns :: TraceViewCat
tvDataDefns = traceView dataDefnTable

tvGenDefns :: TraceViewCat
tvGenDefns = traceView gendefTable

tvTheoryModels :: TraceViewCat
tvTheoryModels = traceView theoryModelTable

tvInsModels :: TraceViewCat
tvInsModels = traceView insmodelTable

tvGoals :: TraceViewCat
tvGoals = traceViewCC goalStmtDom

tvReqs :: TraceViewCat
tvReqs = traceViewCC reqDom

tvChanges :: TraceViewCat
tvChanges = traceViewCC chgProbDom

traceMatAssumpOther :: SystemInformation -> (LabelledContent, [Sentence])
traceMatAssumpOther si = (generateTraceTableView "TraceMatAvsAll"
  (titleize' assumption +:+ S "and Other" +:+ titleize' item) [tvAssumps]
  [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs, tvChanges] si,
  [plural Doc.dataDefn, plural Doc.thModel, plural Doc.genDefn, plural Doc.inModel,
  plural requirement, plural likelyChg, plural unlikelyChg +:+ S "on the" +:+. plural assumption])

traceMatRefinement :: SystemInformation -> (LabelledContent, [Sentence])
traceMatRefinement si = (generateTraceTableView "TraceMatRefvsRef"
  (titleize' item +:+ S "and Other" +:+ titleize' section_)
  [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels]
  [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels] si,
  [plural Doc.dataDefn, plural Doc.thModel, plural Doc.genDefn, plural Doc.inModel +:+. S "with each other"])

traceMatOtherReq :: SystemInformation -> (LabelledContent, [Sentence])
traceMatOtherReq si = (generateTraceTableView "TraceMatAllvsR"
  (x titleize' +:+ S "and Other" +:+ titleize' item)
  [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs] [tvGoals, tvReqs] si,
  [x plural +:+ S "on the" +:+ plural Doc.dataDefn, plural Doc.thModel, plural Doc.genDefn, plural Doc.inModel]) where
    x g = foldl (\a (f,t) -> a `sC'` case traceMReferrers (flip f $ _sysinfodb si) $ _sysinfodb si of
      [] -> mempty
      _ -> g t) mempty [(tvReqs, requirement), (tvGoals, goalStmt)]
    sC' EmptyS b = b
    sC' a EmptyS = a
    sC' a b = sC a b

traceMatStandard :: SystemInformation -> [(LabelledContent, [Sentence])]
traceMatStandard s = map ($ s) [traceMatAssumpOther, traceMatRefinement, traceMatOtherReq]
