module Language.Drasil.DOT.Print where

import Data.List (intercalate)
import System.IO
import System.Directory

{-
Ideally, all the functions in doclang create a traceability table for us.
That means all the heavy lifting of collecting the actual information required to 
autogenerate the .dot files would already be done. The labels also appear on the reference names 
(for example, a data definition shows up as DD:dataDefUID), so we could sort the graph
in that manner. Then, all we need to do is print.

Each traceability matrix should be separate for easier viewability.

To make the graphs look better, we try to group the elements by section type
(eg. datadefs with datadefs, changes with changes, etc.)
by using subgraphs and making each section type level with each other.
The 'dot' method of displaying graphs naturally groups subgraphs.
-}

-- | Type synonym for clarity.
type Colour = String

-- | Holds all important and relevant information for generating a traceability graph.
-- Includes labels, graph edges (directions), and label colours.
data GraphInfo = GI {
    -------------- graph nodes (labels) -------------------------------
    -- | Assumptions.
    assumpLabels :: [String]
    -- | Data definitions.
    , ddLabels   :: [String]
    -- | General definitions.
    , gdLabels   :: [String]
    -- | Theory models.
    , tmLabels   :: [String]
    -- | Instance models.
    , imLabels   :: [String]
    -- | Requirements. Currently cannot differentiate Functional and Non-Functional ones.
    , rLabels    :: [String]
    -- | Goal statements.
    , gsLabels   :: [String]
    -- | Changes. Currently cannot differentiate Likely and Unlikely ones.
    , cLabels    :: [String]

    -------------- graph edges (directions) ---------------------------
    -- | Assumptions dependent on assumptions.
    , directionsAvsA     :: [(String, [String])] 
    -- | Definitions, models, requirements, and changes dependent on assumptions.
    , directionsAvsAll   :: [(String, [String])]
    -- | Definitions and models that are dependent on other definitions and models.
    , directionsRefvsRef :: [(String, [String])]
    -- | Goals and requirements dependent on definitions, models, and other requirements.
    , directionsAllvsR   :: [(String, [String])]
    -- | Definitions, models, requirements, goals, and changes that are dependent on one another.
    , directionsAllvsAll :: [(String, [String])]

    -------------- node colours ---------------------------------------
    -- give the ability to change colours of bubbles within the graph
    , assumpColour :: Colour
    , ddColour     :: Colour
    , gdColour     :: Colour
    , tmColour     :: Colour
    , imColour     :: Colour
    , rColour      :: Colour
    , gsColour     :: Colour
    , cColour      :: Colour
    
    -- may need more information regarding ranking & ordering, but for now I'm just keeping it simple
}

-- | Creates the directory for output, gathers all individual graph output functions and calls them.
outputDot :: FilePath -> GraphInfo -> IO ()
outputDot outputFilePath gi = do
    createDirectoryIfMissing False outputFilePath
    setCurrentDirectory outputFilePath
    mkOutputAvsA gi
    mkOutputAvsAll gi
    mkOutputRefvsRef gi
    mkOutputAllvsR gi
    mkOutputAllvsAll gi

-- | Output function for assumptions dependent on assumptions.
mkOutputAvsA :: GraphInfo -> IO ()
mkOutputAvsA gi = do
    handle <- openFile "avsa.dot" WriteMode
    hPutStrLn handle "digraph avsa {"
    let labels = filterAndGI gi [assumpLabels]
        prefixLabels = ["A"]
        colours = [assumpColour gi]

        allLabels = zip3 labels prefixLabels colours
    outputSub handle (directionsAvsA gi) allLabels
    hPutStrLn handle "}"
    hClose handle

-- | Output function for definitions, models, requirements, and changes dependent on assumptions.
mkOutputAvsAll :: GraphInfo -> IO ()
mkOutputAvsAll gi = do
    handle <- openFile "avsall.dot" WriteMode
    hPutStrLn handle "digraph avsall {"
    let labels = filterAndGI gi [assumpLabels, ddLabels, tmLabels, gdLabels, imLabels, rLabels, cLabels]
        prefixLabels = ["A", "DD", "TM", "GD", "IM", "R", "C"]
        colours = map ($ gi) [assumpColour, ddColour, tmColour, gdColour, imColour, rColour, cColour]
        
        allLabels = zip3 labels prefixLabels colours
    outputSub handle (directionsAvsAll gi) allLabels
    hPutStrLn handle "}"
    hClose handle

-- | Output function for definitions and models that are dependent on other definitions and models.
mkOutputRefvsRef :: GraphInfo -> IO ()
mkOutputRefvsRef gi = do
    handle <- openFile "refvsref.dot" WriteMode
    hPutStrLn handle "digraph refvsref {"
    let labels = filterAndGI gi [ddLabels, tmLabels, gdLabels, imLabels]
        prefixLabels = ["DD", "TM", "GD", "IM"]
        colours = map ($ gi) [ddColour, tmColour, gdColour, imColour]

        allLabels = zip3 labels prefixLabels colours 
    outputSub handle (directionsRefvsRef gi) allLabels
    hPutStrLn handle "}"
    hClose handle

-- | Output function for goals and requirements dependent on definitions, models, and other requirements.
mkOutputAllvsR :: GraphInfo -> IO ()
mkOutputAllvsR gi = do
    handle <- openFile "allvsr.dot" WriteMode
    hPutStrLn handle "digraph allvsr {"
    let labels = filterAndGI gi [assumpLabels, ddLabels, tmLabels, gdLabels, imLabels, rLabels, gsLabels]
        prefixLabels = ["A", "DD", "TM", "GD", "IM", "R", "GS"]
        colours = map ($ gi) [assumpColour, ddColour, tmColour, gdColour, imColour, rColour, gsColour]
        
        allLabels = zip3 labels prefixLabels colours
    outputSub handle (directionsAllvsR gi) allLabels
    hPutStrLn handle "}"
    hClose handle

-- | Output function for definitions, models, requirements, goals, and changes that are dependent on one another.
mkOutputAllvsAll :: GraphInfo -> IO ()
mkOutputAllvsAll gi = do
    handle <- openFile "allvsall.dot" WriteMode
    hPutStrLn handle $ "digraph allvsall {"
    let labels = filterAndGI gi [assumpLabels, ddLabels, tmLabels, gdLabels, imLabels, rLabels, gsLabels, cLabels]
        prefixLabels = ["A", "DD", "TM", "GD", "IM", "R", "GS", "C"]
        colours = map ($ gi) [assumpColour, ddColour, tmColour, gdColour, imColour, rColour, gsColour, cColour]

        allLabels = zip3 labels prefixLabels colours
    outputSub handle (directionsAllvsAll gi) allLabels
    hPutStrLn handle "}"
    hClose handle

-------------
-- General helper functions
-------------

-- | Graph output helper. Takes in the file handle, edges, and nodes.
outputSub :: Handle -> [(String, [String])] -> [([String], String, Colour)] -> IO ()
outputSub handle edges nodes = do
    mapM_ (mkDirections handle) edges
    hPutStrLn handle "\n"
    mapM_ (mkNodes handle) nodes

-- | Prints graph edges (directions) onto a given file handle.
mkDirections :: Handle -> (String, [String]) -> IO ()
mkDirections handle ls = do
    mapM_ (hPutStrLn handle) $ makeEdgesSub (fst ls) (filter (not . null) $ snd ls)
    where
       -- Creates an edge between a type and its dependency (indented for subgraphs)
        makeEdgesSub :: String -> [String] -> [String]
        makeEdgesSub _ [] = []
        makeEdgesSub nm (c:cs) = ("\t" ++ filterInvalidChars nm ++ " -> " ++ filterInvalidChars c ++ ";"): makeEdgesSub nm cs

-- | Prints graph nodes (labels) onto a given file handle.
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

-- | Gets graph labels and removes any invalid characters.
filterAndGI :: GraphInfo -> [GraphInfo -> [String]] -> [[String]]
filterAndGI gi ls = map (map filterInvalidChars) $ map ($ gi) ls

-- | Helper to remove invalid characters.
filterInvalidChars :: String -> String
filterInvalidChars = filter (\l -> not (l `elem` invalidChars))
  where
    invalidChars = "^[]!} (){->,$"