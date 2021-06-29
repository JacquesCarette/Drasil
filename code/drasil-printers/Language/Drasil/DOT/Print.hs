module Language.Drasil.DOT.Print where

--import Drasil.DocLang
import Language.Drasil

-- Brainstorming ideas here
{-
Ideally, all the functions in doclang create a traceability table for us.
That means all the heavy lifting of collecting the actual information required to 
autogenerate the .dot files would already be done. The labels also appear on the reference names 
(for example, a data definition shows up as DD:dataDefUID), so we could sort the graph
in that manner. But then again, Drasil.Sections.TraceabilityMandGs already does most of this stuff.
However, I think that drasil-docLang isn't compiled until after drasil-printers, 
so that could pose a problem.

Also, this has the ability to make the type dependency graph generator way more generalized.
For now though, I think just focus on making them separate but similar.

Some of these functions/types should exist:

-- Maybe use traceMColumns. Use generateTraceTableView for reference?
func1 :: TraceConfig -> GraphInfo
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


data GraphInfo = GI {
    assumptionLabels :: [String]
    dataDefLabels :: [String]
    TMLabels :: [String]
    ...

    directions :: [(String, [String])] -- graph directions
    graphType :: String -- AvsAll graph, AvsA graph, etc.
    sections :: [String] -- can be assumptions, TMs, DDs, etc.
    
    assumpColour :: Colour -- give the ability to change colours of bubbles within the graph
    TMColour :: Colour
    ...
    -- may need more information regarding ranking & ordering, but for now I'm just keeping it simple
}

-- Output files -- mostly already done from the files in the scripts folder.

-- Does this need to know a filepath? How can we put it in the same place as the build files?
-- Is this a drasil-generator type of thing?
output :: GraphInfo -> FilePath -> IO ()
output gi outputFilePath = do
    createDirectoryIfMissing False outputFilePath
    setCurrentDirectory outputFilePath
    handle <- openFile (graphtype gi ++ ".dot") WriteMode
    hPutStrLn handle $ "digraph " ++ graphtype gi ++ " {"
    mapM_ (outputSub gi handle) $ sections gi
    hPutStrLn handle " }"
    hClose typeGraph


-- since the 'dot' method of displaying graphs naturally groups subgraphs,
all related ideas could be grouped by section type 
(eg. assumptions all together, datadefs all together)
outputSub :: GraphInfo -> Handle -> String -> IO ()
outputSub gi handle section = do
    hputStrLn handle ("\tsubgraph " ++ section ++ " {")
    mapM_ (mkDirections handle) (directions gi)
    let allLabels = zip [assumptionLabels gi, dataDefLabels gi, tmLabels gi, ...] [assumpColour gi, dataDefColour gi, ...]
    mapM_ (uncurry (mkNodes handle)) allLabels
    hPutStrLn handle "\t}"

mkDirections :: Handle -> (String, [String]) -> IO ()
mkDirections handle l = do
    mapM_ (hPutStrLn handle) $ (uncurry makeEdgesSub) l
    where
       -- Creates an edge between a type and its dependency (indented for subgraphs)
        makeEdgesSub :: String -> [String] -> [String]
        makeEdgesSub _ [] = []
        makeEdgesSub nm (c:cs) = ("\t\t" ++ nm ++ " -> " ++ c ++ ";"): makeEdgesSub nm cs

mkNodes :: Handle -> [String] -> Colour -> IO ()
mkNodes handle ls col = do
    mapM_ (hPutStrLn handle) $ makeNodesSub col ls
    where
        -- Creates a node based on the kind of datatype (indented for subgraphs)
        makeNodesSub :: Colour -> String -> String
        makeNodesSub c nm = "\t\t" ++ nm ++ "\t[shape=oval, color=" ++ c ++ ", label=" ++ nm ++ "];"
-}