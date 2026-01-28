module Drasil.Generator.SRS.TraceabilityGraphs (outputDot) where

import Data.List (intercalate)
import System.IO (Handle, IOMode(WriteMode), openFile, hPutStrLn, hClose)
import System.Directory (setCurrentDirectory)

import Drasil.Database (UID)
import Drasil.Metadata.TraceabilityGraphs (GraphInfo(..), NodeFamily(..),
  Label, Colour)
import Utils.Drasil (createDirIfMissing)

-- | Creates the directory for output, gathers all individual graph output functions and calls them.
outputDot :: FilePath -> GraphInfo -> IO ()
outputDot outputFilePath gi = do
    createDirIfMissing False outputFilePath
    setCurrentDirectory outputFilePath
    mkOutput gi "avsa" edgesAvsA [assumpNF]
    mkOutput gi "avsall" edgesAvsAll [assumpNF, ddNF, tmNF, gdNF, imNF, reqNF, chgNF]
    mkOutput gi "refvsref" edgesRefvsRef [ddNF, tmNF, gdNF, imNF]
    mkOutput gi "allvsr" edgesAllvsR [assumpNF, ddNF, tmNF, gdNF, imNF, reqNF, gsNF]
    mkOutput gi "allvsall" edgesAllvsAll [assumpNF, ddNF, tmNF, gdNF, imNF, reqNF, gsNF, chgNF]

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
