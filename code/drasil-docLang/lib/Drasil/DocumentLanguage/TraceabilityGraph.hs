{-# LANGUAGE PostfixOperators, TupleSections #-}
-- | Defines functions to create traceability graphs in SRS documents.
module Drasil.DocumentLanguage.TraceabilityGraph where

import Language.Drasil
import Drasil.Database (UID, find, isRegistered, (+++.), mkUid)
import Drasil.Database.SearchTools (termResolve', shortForm)
import Drasil.System hiding (purpose)
import Control.Lens ((^.))
import Drasil.DocumentLanguage.TraceabilityMatrix (TraceViewCat, traceMReferees, traceMReferrers,
  traceMColumns, layoutUIDs, traceMIntro)
import Drasil.Sections.TraceabilityMandGs (tvAssumps,
  tvDataDefns, tvGenDefns, tvTheoryModels, tvInsModels, tvGoals, tvReqs,
  tvChanges)
import qualified Drasil.DocLang.SRS as SRS
import Language.Drasil.Printers (GraphInfo(..), NodeFamily(..))
import Data.Maybe (fromMaybe)
import Data.Drasil.Concepts.Math (graph)
import Data.Drasil.Concepts.Documentation (traceyGraph, component, dependency, reference, purpose, traceyMatrix)
import qualified Language.Drasil.Sentence.Combinators as S
import Data.Char (toLower)
import Drasil.Sections.ReferenceMaterial (emptySectSentPlu)
import Theory.Drasil

-- * Main Functions

-- | Wrapper for 'traceMIntro' and 'traceGIntro'. Turns references ('LabelledContent's),
-- trailing notes ('Sentence's), and any other needed contents to create a Traceability 'Section'.
-- Traceability graphs generate as both a link and a figure for convenience.
traceMGF :: [LabelledContent] -> [Sentence] -> [Contents] -> String -> [Section] -> Section
traceMGF [] [] [] _ = SRS.traceyMandG [mkParagraph $ emptySectSentPlu [traceyMatrix, traceyGraph]]
traceMGF refs trailing otherContents ex = SRS.traceyMandG (traceMIntro refs trailing : otherContents
  ++ map UlC (traceGIntro traceGUIDs (trailing ++ [allvsallDesc])) ++ traceGCon ex)

-- | Generalized traceability graph introduction: appends references to the traceability graphs in 'Sentence' form
-- and wraps in 'Contents'. Usually references the five graphs as defined in 'GraphInfo'.
traceGIntro :: [UID] -> [Sentence] -> [UnlabelledContent]
traceGIntro refs trailings = [ulcc $ Paragraph $ foldlSent
        [phrase purpose `S.the_ofTheC` plural traceyGraph,
        S "is also to provide easy", plural reference, S "on what has to be",
        S "additionally modified if a certain", phrase component +:+. S "is changed",
        S "The arrows in the", plural graph, S "represent" +:+. plural dependency,
        S "The", phrase component, S "at the tail of an arrow is depended on by the",
        phrase component, S "at the head of that arrow. Therefore, if a", phrase component,
        S "is changed, the", plural component, S "that it points to should also be changed"] +:+
        foldlSent_ (zipWith graphShows refs trailings)]

-- | Extracts traceability graph inforomation from filled-in 'System'.
mkGraphInfo :: System -> GraphInfo
mkGraphInfo si = GI {
    assumpNF = mkGraphNodes tvAssumps si "mistyrose"
    , ddNF     = mkGraphNodes tvDataDefns si "paleturquoise1"
    , gdNF     = mkGraphNodes tvGenDefns si "palegreen"
    , tmNF     = mkGraphNodes tvTheoryModels si "pink"
    , imNF     = mkGraphNodes tvInsModels si "khaki1"
    , reqNF    = mkGraphNodes tvReqs si "ivory"
    , gsNF     = mkGraphNodes tvGoals si "darkgoldenrod1"
    , chgNF    = mkGraphNodes tvChanges si "lavender"

    , edgesAvsA     = mkGraphEdges [tvAssumps] [tvAssumps] si
    , edgesAvsAll   = mkGraphEdges [tvAssumps] [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs, tvChanges] si
    , edgesRefvsRef = mkGraphEdges [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels] [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels] si
    , edgesAllvsR   = mkGraphEdges [tvDataDefns, tvTheoryModels,tvGenDefns, tvInsModels, tvReqs] [tvGoals, tvReqs] si
    , edgesAllvsAll = mkGraphEdges [tvAssumps, tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs, tvGoals, tvChanges] [tvAssumps, tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs, tvGoals, tvChanges] si
}

-- * Helper Functions

-- | Gets the node family of a graph based on the given section
-- and system information. Also applies a given colour to the node family.
mkGraphNodes :: TraceViewCat -> System -> String -> NodeFamily
mkGraphNodes entry si col = NF {nodeUIDs = nodeContents, nodeLabels = map (checkUIDRefAdd si) nodeContents, nfLabel = checkNodeContents nodeContents, nfColour = col}
    where
        checkNodeContents :: [UID] -> String
        checkNodeContents [] = ""
        checkNodeContents (x:_) = checkUIDAbbrev si x
        nodeContents = traceMReferees entryF si
        entryF = layoutUIDs [entry] si

-- | Creates the graph edges based on the relation of the first list of sections to the second.
-- Also needs the system information. Return value is of the form (Section, [Dependencies]).
mkGraphEdges :: [TraceViewCat] -> [TraceViewCat] -> System -> [(UID, [UID])]
mkGraphEdges cols rows si = makeTGraph (traceGRowHeader rowf si) (traceMColumns colf rowf si) $ traceMReferees colf si
    where
        colf = layoutUIDs cols si
        rowf = layoutUIDs rows si

-- | Helper for making graph edges. Taken from Utils.Drasil's traceability matrix relation finder.
-- But, instead of marking "X" on two related ideas, it makes them an edge.
makeTGraph :: [UID] -> [[UID]] -> [UID] -> [(UID, [UID])]
makeTGraph rowName rows cols = zip rowName [zipFTable' x cols | x <- rows]
  where
    zipFTable' content = filter (`elem` content)

-- | Checker for uids by finding if the 'UID' is in one of the possible data
-- sets contained in the 'System' database.
checkUID :: UID -> System -> UID
checkUID t si
  | isRegistered t s = t
  | otherwise = error $ show t ++ "Caught."
  where s = si ^. systemdb

-- | Similar to 'checkUID' but prepends domain for labelling.
checkUIDAbbrev :: System -> UID -> String
checkUIDAbbrev si t
  | Just x <- find t s :: Maybe DataDefinition  = abrv x
  | Just x <- find t s :: Maybe InstanceModel   = abrv x
  | Just x <- find t s :: Maybe GenDefn         = abrv x
  | Just x <- find t s :: Maybe TheoryModel     = abrv x
  | Just x <- find t s :: Maybe ConceptInstance = fromMaybe "" $ shortForm $ termResolve' s $ sDom $ cdom x
  | Just _ <- find t s :: Maybe LabelledContent = show t
  | Just _ <- find t s :: Maybe Citation        = ""
  | otherwise = error $ show t ++ "Caught."
  where s = si ^. systemdb

-- | Similar to 'checkUID' but gets reference addresses for display.
checkUIDRefAdd :: System -> UID -> String
checkUIDRefAdd si t
  | Just x <- find t s :: Maybe DataDefinition  = getAdd $ getRefAdd x
  | Just x <- find t s :: Maybe InstanceModel   = getAdd $ getRefAdd x
  | Just x <- find t s :: Maybe GenDefn         = getAdd $ getRefAdd x
  | Just x <- find t s :: Maybe TheoryModel     = getAdd $ getRefAdd x
  | Just x <- find t s :: Maybe ConceptInstance = fromMaybe "" (shortForm $ termResolve' s $ sDom $ cdom x) ++ ":" ++ getAdd (getRefAdd x)
  | Just _ <- find t s :: Maybe LabelledContent = show t
  | Just _ <- find t s :: Maybe Citation        = ""
  | otherwise                                   = error $ show t ++ "Caught."
  where s = si ^. systemdb

-- | Helper that finds the header of a traceability matrix.
-- However, here we use this to get a list of 'UID's for a traceability graph instead.
traceGHeader :: (System -> [UID]) -> System -> [UID]
traceGHeader f c = map (`checkUID` c) $ f c

-- | Helper that finds the headers of the traceability matrix rows.
-- However, here we use this to get a list of 'UID's for a traceability graph instead.
-- This is then used to create the graph edges.
traceGRowHeader :: ([UID] -> [UID]) -> System -> [UID]
traceGRowHeader f = traceGHeader (traceMReferrers f)

-- FIXME: Should take a Reference instead of just a Reference UID
-- | Helper that makes references of the form "@reference@ shows the dependencies of @something@". Only takes a reference `UID` instead of a `Reference`.
graphShows :: UID -> Sentence -> Sentence
graphShows r end = refS (makeFigRef' r) +:+ S "shows the" +:+ plural dependency `S.of_` (end !.)

-- * Functions to Create a Traceability Graphs
--
-- $createTraceyGraphs
--
-- Functions related to setting up the structure and contents of the traceability graphs section.

-- | Description of the @AllvsAll@ traceability graph.
allvsallDesc :: Sentence
allvsallDesc = S "dependencies of assumptions, models, definitions, requirements, goals, and changes with each other"

-- | Create a list of traceability graph references.
traceGLst :: Contents
traceGLst = UlC $ ulcc $ Enumeration $ Bullet $ map (, Nothing) folderList'

-- | The Traceability Graph contents.
traceGCon :: String -> [Contents] -- FIXME: HACK: We're generating "LlC"s of the traceability graphs multiple times... See DocumentLanguage.hs' mkTraceabilitySec for the other spot.
traceGCon ex = map LlC (genTraceGraphLabCons ex)
            ++ [mkParagraph $ S "For convenience, the following graphs can be\
               \ found at the links below:", traceGLst]

-- | Generate the `LabelledContent` chunks, specialized to a specific example
-- (and its path).
genTraceGraphLabCons :: String -> [LabelledContent]
genTraceGraphLabCons ex = zipWith (traceGraphLC ex) traceGFiles traceGUIDs

-- | Generates traceability graphs as figures on an SRS document.
traceGraphLC :: String -> FilePath -> UID -> LabelledContent
traceGraphLC ex fp u = llcc (makeFigRef' u) $ fig (S $ show u) (traceyGraphPath ex fp)

-- | Traceability graph file names.
traceGFiles :: [String]
-- | Traceabiliy graph reference 'UID's.
traceGUIDs :: [UID]
-- | Create reference paths to traceability graphs given an example name. For @.pdf@ links
traceyGraphPaths :: String -> [String]
-- | Create references to traceability graphs given an example name. Primarily used for reference database in examples.
traceyGraphGetRefs :: String -> [Reference]
-- | Gets the path of a traceability graph given an example folder name and the graph name. For @.png@ files
traceyGraphPath :: String -> String -> String

traceGFiles = ["avsa", "avsall", "refvsref", "allvsr", "allvsall"]
traceGUIDs = map mkUid ["TraceGraphAvsA", "TraceGraphAvsAll", "TraceGraphRefvsRef", "TraceGraphAllvsR", "TraceGraphAllvsAll"]
traceyGraphPaths ex = map (\x -> resourcePath ++ map toLower ex ++ "/" ++ x ++ ".svg") traceGFiles
traceyGraphGetRefs ex = map makeFigRef' traceGUIDs ++ zipWith (\x y -> Reference (x +++. "Link") (URI y) (shortname' $ S $ show x)) traceGUIDs (traceyGraphPaths $ map toLower ex)
-- for actual use in creating the graph figures
traceyGraphPath ex f = resourcePath ++ map toLower ex ++ "/" ++ f ++ ".svg"

-- | Traceability graphs reference path.
resourcePath :: String
resourcePath = "../../../../traceygraphs/"

-- | Helper to create a list of traceability graph references.
folderList' :: [ItemType]
folderList' = map (Flat . (\x -> Ref (x +++. "Link") EmptyS None)) traceGUIDs
