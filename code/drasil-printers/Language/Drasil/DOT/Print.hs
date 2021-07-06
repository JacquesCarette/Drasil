module Language.Drasil.DOT.Print where

import Language.Drasil
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
    assumpLabels :: (([UID], [String]), String)
    -- | Data definitions.
    , ddLabels   :: (([UID], [String]), String)
    -- | General definitions.
    , gdLabels   :: (([UID], [String]), String)
    -- | Theory models.
    , tmLabels   :: (([UID], [String]), String)
    -- | Instance models.
    , imLabels   :: (([UID], [String]), String)
    -- | Requirements. Currently cannot differentiate Functional and Non-Functional ones.
    , rLabels    :: (([UID], [String]), String)
    -- | Goal statements.
    , gsLabels   :: (([UID], [String]), String)
    -- | Changes. Currently cannot differentiate Likely and Unlikely ones.
    , cLabels    :: (([UID], [String]), String)

    -------------- graph edges (directions) ---------------------------
    -- | Assumptions dependent on assumptions.
    , directionsAvsA     :: [(UID, [UID])] 
    -- | Definitions, models, requirements, and changes dependent on assumptions.
    , directionsAvsAll   :: [(UID, [UID])]
    -- | Definitions and models that are dependent on other definitions and models.
    , directionsRefvsRef :: [(UID, [UID])]
    -- | Goals and requirements dependent on definitions, models, and other requirements.
    , directionsAllvsR   :: [(UID, [UID])]
    -- | Definitions, models, requirements, goals, and changes that are dependent on one another.
    , directionsAllvsAll :: [(UID, [UID])]

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
    let labels = [assumpLabels]
        colours = [assumpColour]
    mkOutput gi "avsa" directionsAvsA labels colours

-- | Output function for definitions, models, requirements, and changes dependent on assumptions.
mkOutputAvsAll :: GraphInfo -> IO ()
mkOutputAvsAll gi = do
    let labels = [assumpLabels, ddLabels, tmLabels, gdLabels, imLabels, rLabels, cLabels]
        colours = [assumpColour, ddColour, tmColour, gdColour, imColour, rColour, cColour]
    mkOutput gi "avsall" directionsAvsAll labels colours

-- | Output function for definitions and models that are dependent on other definitions and models.
mkOutputRefvsRef :: GraphInfo -> IO ()
mkOutputRefvsRef gi = do
    let labels = [ddLabels, tmLabels, gdLabels, imLabels]
        colours = [ddColour, tmColour, gdColour, imColour]
    mkOutput gi "refvsref" directionsRefvsRef labels colours

-- | Output function for goals and requirements dependent on definitions, models, and other requirements.
mkOutputAllvsR :: GraphInfo -> IO ()
mkOutputAllvsR gi = do
    let labels = [assumpLabels, ddLabels, tmLabels, gdLabels, imLabels, rLabels, gsLabels]
        colours = [assumpColour, ddColour, tmColour, gdColour, imColour, rColour, gsColour]
    mkOutput gi "allvsr" directionsAllvsR labels  colours

-- | Output function for definitions, models, requirements, goals, and changes that are dependent on one another.
mkOutputAllvsAll :: GraphInfo -> IO ()
mkOutputAllvsAll gi = do
    let labels = [assumpLabels, ddLabels, tmLabels, gdLabels, imLabels, rLabels, gsLabels, cLabels]
        colours = [assumpColour, ddColour, tmColour, gdColour, imColour, rColour, gsColour, cColour]
    mkOutput gi "allvsall" directionsAllvsAll labels colours

-------------
-- General helper functions
-------------

-- | General output function for making a traceability graph. Takes in the graph information, title, direction function, label functions, label prefixes, and colour functions.
mkOutput :: GraphInfo -> String -> (GraphInfo -> [(String, [String])]) -> [GraphInfo -> (([String], [String]), String)] -> [GraphInfo -> Colour] -> IO ()
mkOutput gi ttl getDirections getLabels getColours = do
    handle <- openFile (ttl ++ ".dot") WriteMode
    hPutStrLn handle $ "digraph " ++ ttl ++ " {"
    let labels = filterAndGI gi getLabels
        colours = map ($ gi) getColours

        allLabels = zipWith (\x y -> (fst $ fst x, snd $ fst x, snd x, y)) labels colours
    outputSub handle (getDirections gi) allLabels
    hPutStrLn handle "}"
    hClose handle

-- | Graph output helper. Takes in the file handle, edges, and nodes.
outputSub :: Handle -> [(String, [String])] -> [([String], [String], String, Colour)] -> IO ()
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
mkNodes :: Handle -> ([String], [String], String, Colour) -> IO ()
mkNodes handle (ls, lbl, subL, col) = do
    mapM_ ((hPutStrLn handle) . uncurry (makeNodesSub col)) $ zip lbl ls
    hPutStrLn handle $ "\n\tsubgraph " ++ subL ++ " {"
    hPutStrLn handle "\trank=\"same\""
    hPutStrLn handle $ "\t{" ++ intercalate ", " ls ++ "}"
    hPutStrLn handle "\t}\n"
    where
        -- Creates a node based on the kind of datatype (indented for subgraphs)
        makeNodesSub :: Colour -> String -> String -> String
        makeNodesSub c l nm  = "\t" ++ nm ++ "\t[shape=box, color=black, style=filled, fillcolor=" ++ c ++ ", label=\"" ++ l ++ "\"];"

-- | Gets graph labels and removes any invalid characters.
filterAndGI :: GraphInfo -> [GraphInfo -> (([String], [String]), String)] -> [(([String], [String]), String)]
filterAndGI gi ls = map (\x -> ((map filterInvalidChars $ fst $ fst x, snd $ fst x), snd x)) labels
    where
        labels = map ($ gi) ls

-- | Helper to remove invalid characters.
filterInvalidChars :: String -> String
filterInvalidChars = filter (\l -> not (l `elem` invalidChars))
  where
    invalidChars = "^[]!} (){->,$"