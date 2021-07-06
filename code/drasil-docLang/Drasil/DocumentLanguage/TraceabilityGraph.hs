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

    , assumpLabels = (getLabels tvAssumps si,      "A")
    , ddLabels     = (getLabels tvDataDefns si,    "DD")
    , gdLabels     = (getLabels tvGenDefns si,     "GD")
    , tmLabels     = (getLabels tvTheoryModels si, "TM")
    , imLabels     = (getLabels tvInsModels si,    "IM")
    , rLabels      = (getLabels tvReqs si,         "R")
    , gsLabels     = (getLabels tvGoals si,        "GS")
    , cLabels      = (getLabels tvChanges si,      "C")

    , directionsAvsA     = mkGraphEdges [tvAssumps] [tvAssumps] si
    , directionsAvsAll   = mkGraphEdges [tvAssumps] [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs, tvChanges] si
    , directionsRefvsRef = mkGraphEdges [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels] [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels] si
    , directionsAllvsR   = mkGraphEdges [tvDataDefns, tvTheoryModels,tvGenDefns, tvInsModels, tvReqs] [tvGoals, tvReqs] si
    , directionsAllvsAll = mkGraphEdges [tvAssumps, tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs, tvGoals, tvChanges] [tvAssumps, tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs, tvGoals, tvChanges] si
}

{--- | Gets the nodes of a graph based on a list of sections we want to examine and the system information.
mkGraphNodes :: [TraceViewCat] -> SystemInformation -> [UID]
mkGraphNodes entries si = (traceMReferees entryF cdb)
    where
        cdb = _sysinfodb si
        entryF = layoutUIDs entries cdb-}

-- Testing new graphnodes function
-- | Gets the nodes of a graph based on a list of sections we want to examine and the system information.
mkGraphNodes :: TraceViewCat -> SystemInformation -> ([UID], [String])
mkGraphNodes entry si = (nodeContents, map checkUID' nodeContents)
    where
        nodeContents = traceMReferees entryF cdb
        cdb = _sysinfodb si
        entryF = layoutUIDs [entry] cdb

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
    zipFTable' content = filter (`elem` content)

-- | Get all possible nodes based on the system information and a single section.
-- In other words, finds all possible UIDs under a given section.
getLabels :: TraceViewCat -> SystemInformation -> ([UID], [String])
getLabels l si = mkGraphNodes l si

-- | Checker for uids by finding if the 'UID' is in one of the possible data sets contained in the 'SystemInformation' database.
checkUID :: UID -> SystemInformation -> UID
checkUID t si
  | Just _ <- Map.lookupIndex t (s ^. dataDefnTable)        = t
  | Just _ <- Map.lookupIndex t (s ^. insmodelTable)        = t
  | Just _ <- Map.lookupIndex t (s ^. gendefTable)          = t
  | Just _ <- Map.lookupIndex t (s ^. theoryModelTable)     = t
  | Just _ <- Map.lookupIndex t (s ^. conceptinsTable)      = t
  | Just _ <- Map.lookupIndex t (s ^. sectionTable)         = t
  | Just _ <- Map.lookupIndex t (s ^. labelledcontentTable) = t
  | t `elem` map  (^. uid) (citeDB si) = ""
  | otherwise = error $ t ++ "Caught."
  where s = _sysinfodb si

-- have to somehow get the domain abbreviation from here. Maybe mabbr?
checkUID' :: UID -> SystemInformation -> String
checkUID' t si
  | Just _ <- Map.lookupIndex t (s ^. dataDefnTable)        = refS $ datadefnLookup    t (s ^. dataDefnTable)
  | Just _ <- Map.lookupIndex t (s ^. insmodelTable)        = refS $ insmodelLookup    t (s ^. insmodelTable)
  | Just _ <- Map.lookupIndex t (s ^. gendefTable)          = refS $ gendefLookup      t (s ^. gendefTable)
  | Just _ <- Map.lookupIndex t (s ^. theoryModelTable)     = refS $ theoryModelLookup t (s ^. theoryModelTable)
  | Just _ <- Map.lookupIndex t (s ^. conceptinsTable)      = refS $ conceptinsLookup  t (s ^. conceptinsTable)
  | Just _ <- Map.lookupIndex t (s ^. sectionTable)         = refS $ sectionLookup     t (s ^. sectionTable)
  | Just _ <- Map.lookupIndex t (s ^. labelledcontentTable) = refS $ labelledconLookup t (s ^. labelledcontentTable)
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
