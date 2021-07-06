module Drasil.DocumentLanguage.TraceabilityGraph (mkGraphInfo) where

import Language.Drasil
import Database.Drasil hiding (cdb)
import Control.Lens ((^.))
import qualified Data.Map as Map
import Drasil.DocumentLanguage.TraceabilityMatrix (TraceViewCat, traceMReferees, traceMReferrers,
  traceMColumns, ensureItems, layoutUIDs)
import Drasil.Sections.TraceabilityMandGs (tvAssumps,
  tvDataDefns, tvGenDefns, tvTheoryModels, tvInsModels, tvGoals, tvReqs,
  tvChanges)
import Language.Drasil.Printers (GraphInfo(..))

-- | Extracts traceability graph inforomation from filled-in 'SystemInformation'.
mkGraphInfo :: SystemInformation -> GraphInfo
mkGraphInfo si = GI {
      assumpColour = "mistyrose"
    , ddColour     = "paleturquoise1"
    , gdColour     = "palegreen"
    , tmColour     = "pink"
    , imColour     = "khaki1"
    , rColour      = "ivory"
    , gsColour     = "darkgoldenrod1"
    , cColour      = "lavender"

    , assumpLabels = getLabels tvAssumps si
    , ddLabels     = getLabels tvDataDefns si
    , gdLabels     = getLabels tvGenDefns si
    , tmLabels     = getLabels tvTheoryModels si
    , imLabels     = getLabels tvInsModels si
    , rLabels      = getLabels tvReqs si
    , gsLabels     = getLabels tvGoals si
    , cLabels      = getLabels tvChanges si

    , directionsAvsA     = mkGraphEdges [tvAssumps] [tvAssumps] si
    , directionsAvsAll   = mkGraphEdges [tvAssumps] [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs, tvChanges] si
    , directionsRefvsRef = mkGraphEdges [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels] [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels] si
    , directionsAllvsR   = mkGraphEdges [tvDataDefns, tvTheoryModels,tvGenDefns, tvInsModels, tvReqs] [tvGoals, tvReqs] si
    , directionsAllvsAll = mkGraphEdges [tvAssumps, tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs, tvGoals, tvChanges] [tvAssumps, tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs, tvGoals, tvChanges] si
}

-- | Gets the nodes of a graph based on a list of sections we want to examine and the system information.
mkGraphNodes :: [TraceViewCat] -> SystemInformation -> [UID]
mkGraphNodes entries si = (traceMReferees entryF cdb)
    where
        cdb = _sysinfodb si
        entryF = layoutUIDs entries cdb

-- | Creates the graph edges based on the relation of the first list of sections to the second.
-- Also needs the system information. Return value is of the form (Section, [Dependencies])
mkGraphEdges :: [TraceViewCat] -> [TraceViewCat] -> SystemInformation -> [(UID, [UID])]
mkGraphEdges cols rows si = makeTGraph (ensureItems "Traceability Graph" $ traceGRowHeader rowf si) (traceMColumns colf rowf cdb) $ traceMReferees colf cdb
    where
        cdb = _sysinfodb si
        colf = layoutUIDs cols cdb
        rowf = layoutUIDs rows cdb

-- | Helper for making graph edges. Taken from Utils.Drasil's traceability matrix relation finder.
-- But, instead of marking "X" on two related ideas, it makes them an edge.
makeTGraph :: [String] -> [[String]] -> [String] -> [(String, [String])]
makeTGraph rowName rows cols = zip rowName [zipFTable' x cols | x <- rows]
  where
    zipFTable' content = concatMap (\x -> if x `elem` content then [x] else [""])

-- | Get all possible nodes based on the system information and a single section.
-- In other words, finds all possible UIDs under a given section.
getLabels :: TraceViewCat -> SystemInformation -> [UID]
getLabels l si = mkGraphNodes [l] si

-- | Checker for uids by finding if the 'UID' is in one of the possible data sets contained in the 'SystemInformation' database.
checkUID :: UID -> SystemInformation -> UID
checkUID t si
  | t `elem` Map.keys (s ^. dataDefnTable)        = datadefnLookup    t (s ^. dataDefnTable) ^. uid
  | t `elem` Map.keys (s ^. insmodelTable)        = insmodelLookup    t (s ^. insmodelTable) ^. uid
  | t `elem` Map.keys (s ^. gendefTable)          = gendefLookup      t (s ^. gendefTable) ^. uid
  | t `elem` Map.keys (s ^. theoryModelTable)     = theoryModelLookup t (s ^. theoryModelTable) ^. uid
  | t `elem` Map.keys (s ^. conceptinsTable)      = conceptinsLookup  t (s ^. conceptinsTable) ^. uid
  | t `elem` Map.keys (s ^. sectionTable)         = sectionLookup     t (s ^. sectionTable) ^. uid
  | t `elem` Map.keys (s ^. labelledcontentTable) = labelledconLookup t (s ^. labelledcontentTable)  ^. uid
  | t `elem` map  (^. uid) (citeDB si) = ""
  | otherwise = error $ t ++ "Caught."
  where s = _sysinfodb si

-- | Helper that finds the header of a traceability matrix.
-- However, here we use this to get a list of 'UID's for a traceability graph instead.
traceGHeader :: (ChunkDB -> [UID]) -> SystemInformation -> [UID]
traceGHeader f c = map (`checkUID` c) $ f $ _sysinfodb c

-- | Helper that finds the headers of the traceability matrix rows.
-- However, here we use this to get a list of 'UID's for a traceability graph instead.
-- This is then used to create the graph edges.
traceGRowHeader :: ([UID] -> [UID]) -> SystemInformation -> [UID]
traceGRowHeader f = traceGHeader (traceMReferrers f)
