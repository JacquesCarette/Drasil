module Language.Drasil.DOT.Print where

--import Drasil.DocLang
import Language.Drasil
import Database.Drasil hiding (cdb)
--import Data.Char (toLower)
import Data.List (nub, intercalate)
import Control.Lens ((^.), Getting)
import qualified Data.Map as Map
import System.IO
import Data.Drasil.Concepts.Documentation (assumpDom, chgProbDom,
  goalStmtDom, reqDom)
import System.Directory

type Colour = String

{-
Some (many) of these functions should be in DocLang instead, but for simplicity + debugging they are here.

Ideally, all the functions in doclang create a traceability table for us.
That means all the heavy lifting of collecting the actual information required to 
autogenerate the .dot files would already be done. The labels also appear on the reference names 
(for example, a data definition shows up as DD:dataDefUID), so we could sort the graph
in that manner. But then again, Drasil.Sections.TraceabilityMandGs already does most of this stuff.
However, I think that drasil-docLang isn't compiled until after drasil-printers, 
so that could pose a problem.

Each traceability matrix should be separate for easier viewability

Also, this has the ability to make the type dependency graph generator in the scripts folder way more generalized.
For now though, I think just focus on making them separate but similar.

To make the graphs look better, we could try to group the elements by section type (eg. datadefs with datadefs, changes with changes, etc.)
by using subgraphs. The 'dot' method of displaying graphs naturally groups subgraphs.
-}

genDot :: SystemInformation -> IO ()
genDot si = do
    let gi = mkGraphInfo si
    output "TraceyGraph" gi
    return mempty

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
    , everyLabel   = getLabels tvEverything si

    , directionsAvsA     = mkGraphEdges [tvAssumps] [tvAssumps] si
    , directionsAvsAll   = mkGraphEdges [tvAssumps] [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs, tvChanges] si
    , directionsRefvsRef = mkGraphEdges [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels] [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels] si
    , directionsAllvsR   = mkGraphEdges [tvDataDefns, tvTheoryModels,tvGenDefns, tvInsModels, tvReqs] [tvGoals, tvReqs] si
    , directionsAllvsAll = mkGraphEdges [tvAssumps, tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs, tvGoals, tvChanges] [tvAssumps, tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs, tvGoals, tvChanges] si
}


data GraphInfo = GI {
    assumpLabels :: [String] -- Assumption
    , ddLabels   :: [String] -- Data definition
    , gdLabels   :: [String] -- General definition
    , tmLabels   :: [String] -- Theory model
    , imLabels   :: [String] -- Instance model
    , rLabels    :: [String] -- Requirements. Currently cannot differentiated Functional and Non-Functional ones.
    , gsLabels   :: [String] -- Goal statements
    , cLabels    :: [String] -- Changes. Currently cannot differentiated Likely and Unlikely ones.
    , everyLabel :: [String] -- all labels, unused for now since each label has a separate tag

    , directionsAvsA     :: [(String, [String])] -- graph directions
    , directionsAvsAll   :: [(String, [String])]
    , directionsRefvsRef :: [(String, [String])]
    , directionsAllvsR   :: [(String, [String])]
    , directionsAllvsAll :: [(String, [String])]

    , assumpColour :: Colour -- give the ability to change colours of bubbles within the graph
    , ddColour     :: Colour
    , gdColour     :: Colour
    , tmColour     :: Colour
    , imColour     :: Colour
    , rColour      :: Colour
    , gsColour     :: Colour
    , cColour      :: Colour
    
    -- may need more information regarding ranking & ordering, but for now I'm just keeping it simple
}

-- Output files -- mostly already done from the files in the scripts folder.

-- Does this need to know a filepath? How can we put it in the same place as the build files?
-- Is this a drasil-generator type of thing?
-- Output each graph individually
output :: FilePath -> GraphInfo -> IO ()
output outputFilePath gi = do
    createDirectoryIfMissing False outputFilePath
    setCurrentDirectory outputFilePath
    mkOutputAvsA gi
    mkOutputAvsAll gi
    mkOutputRefvsRef gi
    mkOutputAllvsR gi
    mkOutputAllvsAll gi

mkOutputAvsA :: GraphInfo -> IO ()
mkOutputAvsA gi = do
    handle <- openFile "avsa.dot" WriteMode
    hPutStrLn handle "digraph avsa {"
    outputSubAvsA gi handle
    hPutStrLn handle "}"
    hClose handle

outputSubAvsA :: GraphInfo -> Handle -> IO ()
outputSubAvsA gi handle = do
    mapM_ (mkDirections handle) (directionsAvsA gi)

    let labels = [assumpLabels gi]
        prefixLabels = ["A"]
        colours = [assumpColour gi]

        allLabels = zip3 labels prefixLabels colours

    mapM_ (mkNodes handle) allLabels

mkOutputAvsAll :: GraphInfo -> IO ()
mkOutputAvsAll gi = do
    handle <- openFile "avsall.dot" WriteMode
    hPutStrLn handle $ "digraph avsall {"
    outputSubAvsAll gi handle
    hPutStrLn handle "}"
    hClose handle

outputSubAvsAll :: GraphInfo -> Handle -> IO ()
outputSubAvsAll gi handle = do
    mapM_ (mkDirections handle) (directionsAvsAll gi)

    let labels = map ($ gi) [assumpLabels, ddLabels, tmLabels, gdLabels, imLabels, rLabels, cLabels]
        prefixLabels = ["A", "DD", "TM", "GD", "IM", "R", "C"]
        colours = map ($ gi) [assumpColour, ddColour, tmColour, gdColour, imColour, rColour, cColour]
        
        allLabels = zip3 labels prefixLabels colours

    mapM_ (mkNodes handle) allLabels

mkOutputRefvsRef :: GraphInfo -> IO ()
mkOutputRefvsRef gi = do
    handle <- openFile "refvsref.dot" WriteMode
    hPutStrLn handle $ "digraph refvsref {"
    outputSubRefvsRef gi handle
    hPutStrLn handle "}"
    hClose handle

outputSubRefvsRef :: GraphInfo -> Handle -> IO ()
outputSubRefvsRef gi handle = do
    mapM_ (mkDirections handle) (directionsRefvsRef gi)

    let labels = map ($ gi) [ddLabels, tmLabels, gdLabels, imLabels]
        prefixLabels = ["DD", "TM", "GD", "IM"]
        colours = map ($ gi) [ddColour, tmColour, gdColour, imColour]

        allLabels = zip3 labels prefixLabels colours

    mapM_ (mkNodes handle) allLabels


mkOutputAllvsR :: GraphInfo -> IO ()
mkOutputAllvsR gi = do
    handle <- openFile "allvsr.dot" WriteMode
    hPutStrLn handle $ "digraph allvsr {"
    outputSubAllvsR gi handle
    hPutStrLn handle "}"
    hClose handle

outputSubAllvsR :: GraphInfo -> Handle -> IO ()
outputSubAllvsR gi handle = do
    mapM_ (mkDirections handle) (directionsAllvsR gi)

    let labels = map ($ gi) [assumpLabels, ddLabels, tmLabels, gdLabels, imLabels, rLabels, gsLabels]
        prefixLabels = ["A", "DD", "TM", "GD", "IM", "R", "GS"]
        colours = map ($ gi) [assumpColour, ddColour, tmColour, gdColour, imColour, rColour, gsColour]
        
        allLabels = zip3 labels prefixLabels colours

    mapM_ (mkNodes handle) allLabels

mkOutputAllvsAll :: GraphInfo -> IO ()
mkOutputAllvsAll gi = do
    handle <- openFile "allvsall.dot" WriteMode
    hPutStrLn handle $ "digraph allvsall {"
    outputSubAllvsAll gi handle
    hPutStrLn handle "}"
    hClose handle

outputSubAllvsAll :: GraphInfo -> Handle -> IO ()
outputSubAllvsAll gi handle = do
    mapM_ (mkDirections handle) (directionsAllvsAll gi)

    let labels = map ($ gi) [assumpLabels, ddLabels, tmLabels, gdLabels, imLabels, rLabels, gsLabels, cLabels]
        prefixLabels = ["A", "DD", "TM", "GD", "IM", "R", "GS", "C"]
        colours = map ($ gi) [assumpColour, ddColour, tmColour, gdColour, imColour, rColour, gsColour, cColour]

        allLabels = zip3 labels prefixLabels colours

    mapM_ (mkNodes handle) allLabels



mkDirections :: Handle -> (String, [String]) -> IO ()
mkDirections handle ls = do
    mapM_ (hPutStrLn handle) $ makeEdgesSub (fst ls) (filter (not . null) $ snd ls)
    where
       -- Creates an edge between a type and its dependency (indented for subgraphs)
        makeEdgesSub :: String -> [String] -> [String]
        makeEdgesSub _ [] = []
        makeEdgesSub nm (c:cs) = ("\t" ++ nm ++ " -> " ++ c ++ ";"): makeEdgesSub nm cs

mkNodes :: Handle -> ([String], String, Colour) -> IO ()
mkNodes handle (ls, lbl, col) = do
    mapM_ ((hPutStrLn handle) . (makeNodesSub col lbl)) ls
    hPutStrLn handle $ "\n\tsubgraph " ++ lbl ++ " {"
    hPutStrLn handle "\trank=\"same\""
    hPutStrLn handle $ "\t{" ++ intercalate ", " ls ++ "}"
    hPutStrLn handle "\t}\n"
    where
        -- Creates a node based on the kind of datatype (indented for subgraphs)
        makeNodesSub :: Colour -> String -> String -> String
        makeNodesSub c l nm  = "\t" ++ nm ++ "\t[shape=box, color=black, style=filled, fillcolor=" ++ c ++ ", label=\"" ++ l ++ ": " ++ nm ++ "\"];"
        --makeNodesSub c l nm  = "\t" ++ nm ++ "\t[shape=box, color=" ++ c ++ ", label=\"" ++ l ++ nm ++ "\"];"

------------ Modified helper functions ------------
mkGraphNodes :: [TraceViewCat] -> SystemInformation -> [UID]
mkGraphNodes entries si = (traceMReferees entryF cdb)
    where
        cdb = _sysinfodb si
        entryF = layoutUIDs entries cdb

mkGraphEdges :: [TraceViewCat] -> [TraceViewCat] -> SystemInformation -> [(UID, [UID])]
mkGraphEdges cols rows si = makeTGraph (ensureItems $ traceGRowHeader rowf si) (traceMColumns colf rowf cdb) $ traceMReferees colf cdb
    where
        cdb = _sysinfodb si
        colf = layoutUIDs cols cdb
        rowf = layoutUIDs rows cdb

makeTGraph :: [String] -> [[String]] -> [String] -> [(String, [String])]
makeTGraph rowName rows cols = zip rowName [zipFTable' x cols | x <- rows]
  where
    zipFTable' content = concatMap (\x -> if x `elem` content then [x] else [""])

getLabels :: TraceViewCat -> SystemInformation -> [UID]
getLabels l si = mkGraphNodes [l] si

-- | Helper that finds the traceability matrix references (things that are referring to other things).
traceGReferrers :: ([UID] -> [UID]) -> ChunkDB -> [[UID]]
traceGReferrers f = (map f) . nub . Map.elems . (^. refbyTable)

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
traceGHeader :: (ChunkDB -> [UID]) -> SystemInformation -> [UID]
traceGHeader f c = map (`checkUID` c) $ f $ _sysinfodb c

-- | Helper that finds the headers of the traceability matrix rows.
traceGRowHeader :: ([UID] -> [UID]) -> SystemInformation -> [UID]
traceGRowHeader f = traceGHeader (traceMReferrers f)

----------- Helper functions taken from other parts of drasil. Modified versions could be useful here.-----------

-- | Helper that finds the traceability matrix references (things being referenced).
traceMReferees :: ([UID] -> [UID]) -> ChunkDB -> [UID]
traceMReferees f = f . nub . Map.keys . (^. refbyTable)

-- | Helper that finds the traceability matrix references (things that are referring to other things).
traceMReferrers :: ([UID] -> [UID]) -> ChunkDB -> [UID]
traceMReferrers f = f . nub . concat . Map.elems . (^. refbyTable)

-- | Helper that makes the columns of a traceability matrix.
traceMColumns :: ([UID] -> [UID]) -> ([UID] -> [UID]) -> ChunkDB -> [[UID]]
traceMColumns fc fr c = map ((\u -> filter (`elem` u) $ fc u) . flip traceLookup (c ^. traceTable)) $ traceMReferrers fr c

-- | Helper type that takes two sets of 'UID's and a 'ChunkDB'.
type TraceViewCat = [UID] -> ChunkDB -> [UID]

-- | Helper that makes sure the rows and columns of a traceability matrix have substance.
ensureItems :: [a] -> [a]
ensureItems [] = error $ "Expected non-empty matrix dimension for traceability matrix."
ensureItems l = l

-- | Helper that finds the layout 'UID's of a traceability matrix.
layoutUIDs :: [TraceViewCat] -> ChunkDB -> [UID] -> [UID]
layoutUIDs a c e = filter (`elem` (Map.keys $ c ^. traceTable)) $ concatMap (\x -> x e c) a

-- | Helper that filters a traceability matrix given a function.
traceViewFilt :: HasUID a => (a -> Bool) -> Getting (UMap a) ChunkDB (UMap a) -> TraceViewCat
traceViewFilt f table _ = map (^. uid) . filter f . asOrderedList . (^. table)

-- | Helper that is similar to 'traceViewFilt', but the filter is always 'True'.
traceView :: HasUID a => Getting (UMap a) ChunkDB (UMap a) -> TraceViewCat
traceView = traceViewFilt (const True)

-- | Turns a 'Concept' into a 'TraceViewCat' via its domain.
traceViewCC :: Concept c => c -> TraceViewCat
traceViewCC dom u c = traceViewFilt (isDomUnder (dom ^. uid) . sDom . cdom) conceptinsTable u c
  where
    isDomUnder :: UID -> UID -> Bool
    isDomUnder filtDom curr
      | filtDom == curr = True
      | not $ null $ getDom curr = isDomUnder filtDom (sDom $ getDom curr)
      | otherwise = False
    getDom :: UID -> [UID]
    getDom curr = cdom $ defResolve c curr

-- | Traceabiliy viewing everything. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvEverything :: TraceViewCat
tvEverything = flip (const id)
-- | Traceabiliy viewing assumptions. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvAssumps :: TraceViewCat
tvAssumps = traceViewCC assumpDom
-- | Traceabiliy viewing data definitions. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvDataDefns :: TraceViewCat
tvDataDefns = traceView dataDefnTable
-- | Traceabiliy viewing general definitions. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvGenDefns :: TraceViewCat
tvGenDefns = traceView gendefTable
-- | Traceabiliy viewing theory models. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvTheoryModels :: TraceViewCat
tvTheoryModels = traceView theoryModelTable
-- | Traceabiliy viewing instance models. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvInsModels :: TraceViewCat
tvInsModels = traceView insmodelTable
-- | Traceabiliy viewing goals. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvGoals :: TraceViewCat
tvGoals = traceViewCC goalStmtDom
-- | Traceabiliy viewing requirements. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvReqs :: TraceViewCat
tvReqs = traceViewCC reqDom
-- | Traceabiliy viewing changes. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvChanges :: TraceViewCat
tvChanges = traceViewCC chgProbDom