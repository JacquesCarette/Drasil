module Language.Drasil.DOT.Print where

--import Drasil.DocLang
import Language.Drasil
import Database.Drasil
import Data.Char (toLower)
import Data.List (nub)
import Control.Lens ((^.), Getting)
import qualified Data.Map as Map
import System.IO
import Data.Drasil.Concepts.Documentation (assumption, assumpDom, chgProbDom,
  goalStmt, goalStmtDom, requirement, reqDom, item, section_, likelyChg,
  unlikelyChg)
import System.Directory

type Colour = String

-- Brainstorming ideas here
{-
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

Some of these functions/types should exist:-}

genDot :: SystemInformation -> IO ()
genDot si = do
    let gi = mkGraphInfo si
    output "code/TraceyGraph" gi
    return mempty

mkGraphEdges :: [TraceViewCat] -> SystemInformation -> [(UID, [UID])]
mkGraphEdges entries si = zip (traceGReferees entryF cdb) (traceGReferrers entryF cdb)
    where
        cdb = _sysinfodb si
        entryF = layoutUIDs entries cdb

mkGraphInfo :: SystemInformation -> GraphInfo
mkGraphInfo si = GI {
      assumpColour = "red"
    , ddColour = "blue"
    , gdColour = "magenta"
    , tmColour = "green"
    , imColour = "yellow"
    , frColour = "grey"
    , nfrColour = "pink"
    , rColour = "pink"
    , gsColour = "orange"
    , lcColour = "brown"
    , ucColour = "teal"
    , cColour = "teal"

    , assumpLabels = getLabels tvAssumps si
    , ddLabels = getLabels tvDataDefns si
    , gdLabels = getLabels tvGenDefns si
    , tmLabels = getLabels tvTheoryModels si
    , imLabels = getLabels tvInsModels si
    , rLabels = getLabels tvReqs si
    , gsLabels = getLabels tvGoals si
    , cLabels = getLabels tvChanges si
    , allLabels = getLabels tvEverything si

    , directionsAvsA = mkGraphEdges [tvAssumps] si
    , directionsAvsAll = mkGraphEdges [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs, tvChanges] si
    , directionsRefvsRef = mkGraphEdges [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels] si
    , directionsAllvsR = mkGraphEdges [tvGoals, tvReqs] si
    , graphType = AvsA
    , sections = ["A", "DD", "GD", "TM", "IM", "FR", "NFR", "GS", "LC", "UC"]
}

getLabels :: TraceViewCat -> SystemInformation -> [UID]
getLabels l si = map fst $ mkGraphEdges [l] si

-- | Helper that finds the traceability matrix references (things being referenced).
traceGReferees :: ([UID] -> [UID]) -> ChunkDB -> [UID]
traceGReferees f = f . nub . Map.keys . (^. refbyTable)

-- | Helper that finds the traceability matrix references (things that are referring to other things).
traceGReferrers :: ([UID] -> [UID]) -> ChunkDB -> [[UID]]
traceGReferrers f = (map f) . nub . Map.elems . (^. refbyTable)

{-[] _ _ _ = error $ "Expected non-empty list of column-view categories for traceability matrix " ++ u
mkGraphInfo _ [] _ _ = error $ "Expected non-empty list of row-view categories for traceability matrix " ++ u
mkGraphInfo cols rows c gi =
  -----------------zip (ensureItems (traceMColHeader colf c)) --Header row. This is where all the arrows will be pointing from
  makeTGraph (ensureItems $ traceMRowHeader rowf c) (traceMColumns colf rowf cdb) $ traceMReferees colf cdb -- data (rows)
  where
    cdb = _sysinfodb c
    colf = layoutUIDs cols cdb
    rowf = layoutUIDs rows cdb

makeTGraph :: [[String]] -> [String] -> [(String,[String])]
makeTGraph sections possibleDependencies = map (uncurry mTGraphAux) (zip sections possibleDependencies)
  where
    mTGraphAux :: String -> [String] -> (String, [String])
    mtGraphAux sec pDep = if sec `elem` pDep then
  map (\x -> zipFTable' x dep) indep --[zipFTable' x cols | x <- rows]
  where
    zipFTable' independent = concatMap (\x -> if x `elem` independent then x else [])
-- | Generates a traceability table. Takes a 'UID' for the table, a description ('Sentence'), columns ('TraceViewCat'), rows ('TraceViewCat'), and 'SystemInformation'.
generateTraceTableView :: UID -> Sentence -> [TraceViewCat] -> [TraceViewCat] -> SystemInformation -> LabelledContent
generateTraceTableView u _ [] _ _ = error $ "Expected non-empty list of column-view categories for traceability matrix " ++ u
generateTraceTableView u _ _ [] _ = error $ "Expected non-empty list of row-view categories for traceability matrix " ++ u
generateTraceTableView u desc cols rows c = llcc (makeTabRef u) $ Table
  (EmptyS : ensureItems u (traceMColHeader colf c))
  (makeTMatrix (ensureItems u $ traceMRowHeader rowf c) (traceMColumns colf rowf cdb) $ traceMReferees colf cdb)
  (showingCxnBw traceyMatrix desc) True where
    cdb = _sysinfodb c
    colf = layoutUIDs cols cdb
    rowf = layoutUIDs rows cdb-}

data GraphType = AvsA | AvsAll | RefvsRef | AllvsR deriving (Show)

data GraphInfo = GI {
    assumpLabels :: [String] -- Assumption
    , ddLabels :: [String] -- Data definition
    , gdLabels :: [String] -- General definition
    , tmLabels :: [String] -- Theory model
    , imLabels :: [String] -- Instance model
    , frLabels :: [String] -- Functional requirements
    , nfrLabels :: [String] -- Non-functional requirements
    , rLabels :: [String] -- requirements
    , gsLabels :: [String] -- Goal statements
    , lcLabels :: [String] -- Likely changes
    , ucLabels :: [String] -- Unlikely changes
    , cLabels :: [String] -- changes
    , allLabels :: [String] -- all labels

    , directionsAvsA :: [(String, [String])] -- graph directions
    , directionsAvsAll :: [(String, [String])]
    , directionsRefvsRef :: [(String, [String])]
    , directionsAllvsR :: [(String, [String])]
    , graphType :: GraphType -- AvsAll graph, AvsA graph, etc. -----may not be needed?
    , sections :: [String] -- can be assumptions, TMs, DDs, etc.

    , assumpColour :: Colour -- give the ability to change colours of bubbles within the graph
    , ddColour :: Colour
    , gdColour :: Colour
    , tmColour :: Colour
    , imColour :: Colour
    , frColour :: Colour
    , nfrColour :: Colour
    , rColour :: Colour
    , gsColour :: Colour
    , lcColour :: Colour
    , ucColour :: Colour
    , cColour :: Colour
    
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
    mkOutputAvsA outputFilePath gi
    mkOutputAvsAll outputFilePath gi
    mkOutputRefvsRef outputFilePath gi
    mkOutputAllvsR outputFilePath gi

mkOutputAvsA :: FilePath -> GraphInfo -> IO ()
mkOutputAvsA outputFilePath gi = do
    handle <- openFile ((map toLower $ show $ graphType gi) ++ ".dot") WriteMode
    hPutStrLn handle $ "digraph " ++ (map toLower $ show $ graphType gi) ++ " {"
    mapM_ (outputSubAvsA gi handle) $ sections gi
    hPutStrLn handle "}"
    hClose handle

-- since the 'dot' method of displaying graphs naturally groups subgraphs,
--all related ideas could be grouped by section type 
--(eg. assumptions all together, datadefs all together)
outputSubAvsA :: GraphInfo -> Handle -> String -> IO ()
outputSubAvsA gi handle section = do
    hPutStrLn handle ("\tsubgraph " ++ section ++ " {")
    mapM_ (mkDirections handle) (directionsAvsA gi)
    let allLabels = zip [assumpLabels gi] [assumpColour gi]
    mapM_ (uncurry (mkNodes handle)) allLabels
    hPutStrLn handle "\t}"

mkOutputAvsAll :: FilePath -> GraphInfo -> IO ()
mkOutputAvsAll outputFilePath gi = do
    handle <- openFile ((map toLower $ show $ graphType gi) ++ ".dot") WriteMode
    hPutStrLn handle $ "digraph " ++ (map toLower $ show $ graphType gi) ++ " {"
    mapM_ (outputSubAvsA gi handle) $ sections gi
    hPutStrLn handle "}"
    hClose handle

outputSubAvsAll :: GraphInfo -> Handle -> String -> IO ()
outputSubAvsAll gi handle section = do
    hPutStrLn handle ("\tsubgraph " ++ section ++ " {")
    mapM_ (mkDirections handle) (directionsAvsAll gi)
    --let allLabels = zip [assumpLabels gi, ddLabels gi, tmLabels gi, gdLabels gi, imLabels gi, frLabels gi, nfrLabels gi, lcLabels gi, ucLabels gi] [assumpColour gi, ddColour gi, tmColour gi, gdColour gi, imColour gi, frColour gi, nfrColour gi, lcColour gi, ucColour gi]
    let allLabels = zip [assumpLabels gi, ddLabels gi, tmLabels gi, gdLabels gi, imLabels gi, rLabels gi, cLabels gi] [assumpColour gi, ddColour gi, tmColour gi, gdColour gi, imColour gi, rColour gi, cColour gi]
    mapM_ (uncurry (mkNodes handle)) allLabels
    hPutStrLn handle "\t}"

mkOutputRefvsRef :: FilePath -> GraphInfo -> IO ()
mkOutputRefvsRef outputFilePath gi = do
    handle <- openFile ((map toLower $ show $ graphType gi) ++ ".dot") WriteMode
    hPutStrLn handle $ "digraph " ++ (map toLower $ show $ graphType gi) ++ " {"
    mapM_ (outputSubAvsA gi handle) $ sections gi
    hPutStrLn handle "}"
    hClose handle

outputSubRefvsRef :: GraphInfo -> Handle -> String -> IO ()
outputSubRefvsRef gi handle section = do
    hPutStrLn handle ("\tsubgraph " ++ section ++ " {")
    mapM_ (mkDirections handle) (directionsRefvsRef gi)
    let allLabels = zip [ddLabels gi, tmLabels gi, gdLabels gi, imLabels gi] [ddColour gi, tmColour gi, gdColour gi, imColour gi]
    mapM_ (uncurry (mkNodes handle)) allLabels
    hPutStrLn handle "\t}"


mkOutputAllvsR :: FilePath -> GraphInfo -> IO ()
mkOutputAllvsR outputFilePath gi = do
    handle <- openFile ((map toLower $ show $ graphType gi) ++ ".dot") WriteMode
    hPutStrLn handle $ "digraph " ++ (map toLower $ show $ graphType gi) ++ " {"
    mapM_ (outputSubAvsA gi handle) $ sections gi
    hPutStrLn handle "}"
    hClose handle

outputSubAllvsR :: GraphInfo -> Handle -> String -> IO ()
outputSubAllvsR gi handle section = do
    hPutStrLn handle ("\tsubgraph " ++ section ++ " {")
    mapM_ (mkDirections handle) (directionsAllvsR gi) ----------------- map ($ gi) [assumpLabels ..]?
    let allLabels = zip [assumpLabels gi, ddLabels gi, tmLabels gi, gdLabels gi, imLabels gi, frLabels gi, nfrLabels gi, gsLabels gi] [assumpColour gi, ddColour gi, tmColour gi, gdColour gi, imColour gi, frColour gi, nfrColour gi, gsColour gi]
    mapM_ (uncurry (mkNodes handle)) allLabels
    hPutStrLn handle "\t}"

{-- since the 'dot' method of displaying graphs naturally groups subgraphs,
all related ideas could be grouped by section type 
(eg. assumptions all together, datadefs all together)
outputSub :: GraphInfo -> Handle -> String -> IO ()
outputSub gi handle section = do
    hPutStrLn handle ("\tsubgraph " ++ section ++ " {")
    mapM_ (mkDirections handle) (directions gi)
    let allLabels = zip [assumpLabels gi, ddLabels gi, tmLabels gi, ...] [assumpColour gi, ddColour gi, ...]
    mapM_ (uncurry (mkNodes handle)) allLabels
    hPutStrLn handle "\t}"-}

mkDirections :: Handle -> (String, [String]) -> IO ()
mkDirections handle ls = do
    mapM_ (hPutStrLn handle) $ (uncurry makeEdgesSub) ls
    where
       -- Creates an edge between a type and its dependency (indented for subgraphs)
        makeEdgesSub :: String -> [String] -> [String]
        makeEdgesSub _ [] = []
        makeEdgesSub nm (c:cs) = ("\t\t" ++ nm ++ " -> " ++ c ++ ";"): makeEdgesSub nm cs

mkNodes :: Handle -> [String] -> Colour -> IO ()
mkNodes handle ls col = do
    mapM_ ((hPutStrLn handle) . (makeNodesSub col)) ls
    where
        -- Creates a node based on the kind of datatype (indented for subgraphs)
        makeNodesSub :: Colour -> String -> String
        makeNodesSub c nm = "\t\t" ++ nm ++ "\t[shape=oval, color=" ++ c ++ ", label=" ++ nm ++ "];"


----------- Helper functions taken from other parts of drasil. Modified versions could be useful here.-----------

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

-- | Helper that finds the traceability matrix references (things being referenced).
traceMReferees :: ([UID] -> [UID]) -> ChunkDB -> [UID]
traceMReferees f = f . nub . Map.keys . (^. refbyTable)

-- | Helper that finds the traceability matrix references (things that are referring to other things).
traceMReferrers :: ([UID] -> [UID]) -> ChunkDB -> [UID]
traceMReferrers f = f . nub . concat . Map.elems . (^. refbyTable)

-- | Helper that finds the header of a traceability matrix.
traceMHeader :: (ChunkDB -> [UID]) -> SystemInformation -> [UID]
traceMHeader f c = map (`checkUID` c) $ f $ _sysinfodb c

-- | Helper that finds the headers of the traceability matrix columns.
traceMColHeader :: ([UID] -> [UID]) -> SystemInformation -> [UID]
traceMColHeader f = traceMHeader (traceMReferees f)

-- | Helper that finds the headers of the traceability matrix rows.
traceMRowHeader :: ([UID] -> [UID]) -> SystemInformation -> [UID]
traceMRowHeader f = traceMHeader (traceMReferrers f)

-- | Helper that makes the columns of a traceability matrix.
traceMColumns :: ([UID] -> [UID]) -> ([UID] -> [UID]) -> ChunkDB -> [[UID]]
traceMColumns fc fr c = map ((\u -> filter (`elem` u) $ fc u) . flip traceLookup (c ^. traceTable)) $ traceMReferrers fr c

{-- | Generates a traceability table. Takes a 'UID' for the table, a description ('Sentence'), columns ('TraceViewCat'), rows ('TraceViewCat'), and 'SystemInformation'.
generateTraceTableView :: UID -> Sentence -> [TraceViewCat] -> [TraceViewCat] -> SystemInformation -> LabelledContent
generateTraceTableView u _ [] _ _ = error $ "Expected non-empty list of column-view categories for traceability matrix " ++ u
generateTraceTableView u _ _ [] _ = error $ "Expected non-empty list of row-view categories for traceability matrix " ++ u
generateTraceTableView u desc cols rows c = llcc (makeTabRef u) $ Table
  (EmptyS : ensureItems u (traceMColHeader colf c))
  (makeTMatrix (ensureItems u $ traceMRowHeader rowf c) (traceMColumns colf rowf cdb) $ traceMReferees colf cdb)
  (showingCxnBw traceyMatrix desc) True where
    cdb = _sysinfodb c
    colf = layoutUIDs cols cdb
    rowf = layoutUIDs rows cdb-}

-- | Helper type that takes two sets of 'UID's and a 'ChunkDB'.
type TraceViewCat = [UID] -> ChunkDB -> [UID]

-- | Helper that makes sure the rows and columns of a traceability matrix have substance.
ensureItems :: UID -> [a] -> [a]
ensureItems u [] = error $ "Expected non-empty matrix dimension for traceability matrix " ++ u
ensureItems _ l = l

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