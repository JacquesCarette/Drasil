-- | Defines printer types and functions for generating traceability graphs (as .dot files).
module Language.Drasil.DOT.Print where

import Data.List (intercalate)
import System.IO (Handle, IOMode(WriteMode), openFile, hPutStrLn, hClose)
import System.Directory (setCurrentDirectory)

import Drasil.Database (UID)
import Utils.Drasil (createDirIfMissing)

-- * Types

-- | Type synonym for clarity.
type Colour = String
-- | Type synonym for clarity.
type Label = String

-- | A node family contains a list of 'UID's, their display labels, general subgraph label, and colour.
data NodeFamily = NF {
    -- | Node 'UID's.
    nodeUIDs :: [UID]
    -- | Display labels for nodes. We use the reference addresses from the 'UID's.
    , nodeLabels :: [Label]
    -- | Individual subgraph labels. These labels do not show on the
    -- final generated pdf or png files.
    , nfLabel :: Label
    -- | Gives the ability to change colours of bubbles within the graph.
    , nfColour :: Colour
}

-- | Holds all important and relevant information for generating a traceability graph.
-- Includes nodes, graph edges, and node family information.
data GraphInfo = GI {
    --------------- graph node families -------------------------------
    -- | Assumptions.
    assumpNF :: NodeFamily
    -- | Data definitions.
    , ddNF :: NodeFamily
    -- | General definitions.
    , gdNF :: NodeFamily
    -- | Theory models.
    , tmNF :: NodeFamily
    -- | Instance models.
    , imNF :: NodeFamily
    -- | Requirements (both functional and non-functional).
    , reqNF :: NodeFamily
    -- | Goal statement.
    , gsNF :: NodeFamily
    -- | Changes (both likely and unlikely).
    , chgNF :: NodeFamily

    -------------- graph edges  ---------------------------
    -- | Assumptions dependent on assumptions.
    , edgesAvsA     :: [(UID, [UID])]
    -- | Definitions, models, requirements, and changes dependent on assumptions.
    , edgesAvsAll   :: [(UID, [UID])]
    -- | Definitions and models that are dependent on other definitions and models.
    , edgesRefvsRef :: [(UID, [UID])]
    -- | Goals and requirements dependent on definitions, models, and other requirements.
    , edgesAllvsR   :: [(UID, [UID])]
    -- | Definitions, models, requirements, goals, and changes that are dependent on one another.
    , edgesAllvsAll :: [(UID, [UID])]

    -- may need more information regarding ranking & ordering, but for now I'm just keeping it simple
}

-- * Functions
-- ** Main Outputs

-- | Creates the directory for output, gathers all individual graph output functions and calls them.
outputDot :: FilePath -> GraphInfo -> IO ()
outputDot outputFilePath gi = do
    createDirIfMissing False outputFilePath
    setCurrentDirectory outputFilePath
    mkOutputAvsA gi
    mkOutputAvsAll gi
    mkOutputRefvsRef gi
    mkOutputAllvsR gi
    mkOutputAllvsAll gi

-- | Output function for assumptions dependent on assumptions.
mkOutputAvsA :: GraphInfo -> IO ()
mkOutputAvsA gi = do
    let labels = [assumpNF]
    mkOutput gi "avsa" edgesAvsA labels

-- | Output function for definitions, models, requirements, and changes dependent on assumptions.
mkOutputAvsAll :: GraphInfo -> IO ()
mkOutputAvsAll gi = do
    let labels = [assumpNF, ddNF, tmNF, gdNF, imNF, reqNF, chgNF]
    mkOutput gi "avsall" edgesAvsAll labels

-- | Output function for definitions and models that are dependent on other definitions and models.
mkOutputRefvsRef :: GraphInfo -> IO ()
mkOutputRefvsRef gi = do
    let labels = [ddNF, tmNF, gdNF, imNF]
    mkOutput gi "refvsref" edgesRefvsRef labels

-- | Output function for goals and requirements dependent on definitions, models, and other requirements.
mkOutputAllvsR :: GraphInfo -> IO ()
mkOutputAllvsR gi = do
    let labels = [assumpNF, ddNF, tmNF, gdNF, imNF, reqNF, gsNF]
    mkOutput gi "allvsr" edgesAllvsR labels

-- | Output function for definitions, models, requirements, goals, and changes that are dependent on one another.
mkOutputAllvsAll :: GraphInfo -> IO ()
mkOutputAllvsAll gi = do
    let labels = [assumpNF, ddNF, tmNF, gdNF, imNF, reqNF, gsNF, chgNF]
    mkOutput gi "allvsall" edgesAllvsAll labels

-- ** Helpers

-- | General output function for making a traceability graph. Takes in the graph information, title, edge generator functions, and node family functions.
mkOutput :: GraphInfo -> String -> (GraphInfo -> [(UID, [UID])]) -> [GraphInfo -> NodeFamily] -> IO ()
mkOutput gi ttl getDirections getLabels = do
    handle <- openFile (ttl ++ ".dot") WriteMode
    hPutStrLn handle $ "digraph " ++ ttl ++ " {"
    let labels = filterAndGI gi getLabels
    outputSub handle (getDirections gi) labels
    hPutStrLn handle "}"
    hClose handle

-- | Graph output helper. Takes in the file handle, edges, and node families.
outputSub :: Handle -> [(UID, [UID])] -> [NodeFamily] -> IO ()
outputSub handle edges nodes = do
    mapM_ (mkDirections handle) edges
    hPutStrLn handle "\n"
    mapM_ (mkNodes handle) nodes

-- | Prints graph edges (directions) onto a given file handle.
mkDirections :: Handle -> (UID, [UID]) -> IO ()
mkDirections handle ls = do
    mapM_ (hPutStrLn handle) $ makeEdgesSub (show $ fst ls) (filter (not . null) $ map show $ snd ls)
    where
       -- Creates an edge between a type and its dependency (indented for subgraphs)
        makeEdgesSub :: String -> [String] -> [String]
        makeEdgesSub _ [] = []
        makeEdgesSub nm (c:cs) = ("\t" ++ filterInvalidChars nm ++ " -> " ++ filterInvalidChars c ++ ";"): makeEdgesSub nm cs

-- | Prints graph nodes (labels) onto a given file handle.
mkNodes :: Handle -> NodeFamily -> IO ()
mkNodes handle NF{nodeUIDs = u, nodeLabels = ls, nfLabel = lbl, nfColour = col} = do
    mapM_ (hPutStrLn handle . uncurry (makeNodesSub col)) $ zip ls $ map show u
    mkSubgraph handle lbl u
    where
        -- Creates a node based on the kind of datatype (indented for subgraphs)
        makeNodesSub :: Colour -> String -> String -> String
        makeNodesSub c l nm  = "\t" ++ filterInvalidChars nm ++ "\t[shape=box, color=black, style=filled, fillcolor=" ++ c ++ ", label=\"" ++ l ++ "\"];"

-- | Helper that only makes a subgraph if there are elements in the subgraph. Otherwise, it returns nothing.
mkSubgraph :: Handle -> Label -> [UID] -> IO ()
mkSubgraph handle l u
    | null l = return mempty
    | otherwise = do
             hPutStrLn handle $ "\n\tsubgraph " ++ l ++ " {"
             hPutStrLn handle "\trank=\"same\""
             hPutStrLn handle $ "\t{" ++ intercalate ", " (map (filterInvalidChars . show) u) ++ "}"
             hPutStrLn handle "\t}\n"

-- | Gets graph labels.
filterAndGI :: GraphInfo -> [GraphInfo -> NodeFamily] -> [NodeFamily]
filterAndGI gi toNodes = labels
    where
        labels = map ($ gi) toNodes

-- | Helper to remove invalid characters.
filterInvalidChars :: String -> String
filterInvalidChars = filter (`notElem` invalidChars)
  where
    invalidChars = "^[]!} (){->,$"
