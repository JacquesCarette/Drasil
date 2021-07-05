module Language.Drasil.DOT.Print where

import Data.List (intercalate)
import System.IO
import System.Directory

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

type Colour = String

data GraphInfo = GI {
    assumpLabels :: [String] -- Assumption
    , ddLabels   :: [String] -- Data definition
    , gdLabels   :: [String] -- General definition
    , tmLabels   :: [String] -- Theory model
    , imLabels   :: [String] -- Instance model
    , rLabels    :: [String] -- Requirements. Currently cannot differentiated Functional and Non-Functional ones.
    , gsLabels   :: [String] -- Goal statements
    , cLabels    :: [String] -- Changes. Currently cannot differentiated Likely and Unlikely ones.

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

-- Does this need to know a filepath? How can we put it in the same place as the build files?
-- Is this a drasil-generator type of thing?
-- Output each graph individually
outputDot :: FilePath -> GraphInfo -> IO ()
outputDot outputFilePath gi = do
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

    hPutStrLn handle "\n"
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

    hPutStrLn handle "\n"
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

    hPutStrLn handle "\n"
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

    hPutStrLn handle "\n"
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

    hPutStrLn handle "\n"
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