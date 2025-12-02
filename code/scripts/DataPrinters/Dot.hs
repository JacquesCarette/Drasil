-- To generalize the use of dot printers in generating graphs
module DataPrinters.Dot (digraph, subgraph,
  makeEdgesDi, makeEdgesSub, makeNodesDi, makeNodesSub, replaceInvalidChars) where

import System.IO

-- type synonyms for clarity.
type Name = String
type Nodes = (Colour, [String])
type Edges = (String, [String])

-- Make a simple directional graph.
-- Takes in a file handle, name of directional graph, nodes and edges.
digraph :: Handle -> Name -> [Nodes] -> [Edges] -> IO ()
digraph handle nm nds edgs = do
    hPutStrLn handle $ "digraph " ++ replaceInvalidChars nm ++ "{"
    mapM_ (hPutStrLn handle) $ concatMap (\ns -> map (makeNodesDi $ fst ns) $ snd ns) nds
    mapM_ (hPutStrLn handle) $ concatMap (uncurry makeEdgesDi) edgs
    hPutStrLn handle "}"
    hClose handle

-- Similar to digraph, but makes a subgraph and indents each line to look nicer.
-- Does not close the handle when done.
subgraph :: Handle -> Name -> [Nodes] -> [Edges] -> IO ()
subgraph handle nm nds edgs = do
    hPutStrLn handle $ "\t\tsubgraph " ++ replaceInvalidChars nm ++ "{"
    mapM_ (hPutStrLn handle) $ concatMap (\ns -> map (makeNodesSub $ fst ns) $ snd ns) nds
    mapM_ (hPutStrLn handle) $ concatMap (uncurry makeEdgesSub) edgs
    hPutStrLn handle "\t\t}"

------------------
-- Graph-related functions
------------------
type Colour = String

-- Creates an edge between a type and its dependency
makeEdgesDi :: String -> [String] -> [String]
makeEdgesDi nm = map (\c -> replaceInvalidChars nm ++ " -> " ++ replaceInvalidChars c ++ ";")

-- Creates an edge between a type and its dependency (indented for subgraphs)
makeEdgesSub :: String -> [String] -> [String]
makeEdgesSub nm = map (\c -> "\t\t" ++ replaceInvalidChars nm ++ " -> " ++ replaceInvalidChars c ++ ";")

-- Creates a node based on the kind of datatype
makeNodesDi :: Colour -> String -> String
makeNodesDi c nm = replaceInvalidChars nm ++ "\t[shape=oval, color=" ++ c ++ ", label=\"" ++ nm ++ "\"];"

-- Creates a node based on the kind of datatype (indented for subgraphs)
makeNodesSub :: Colour -> String -> String
makeNodesSub c nm = "\t\t" ++ replaceInvalidChars nm ++ "\t[shape=oval, color=" ++ c ++ ", label=\"" ++ nm ++ "\"];"

-- Filter out invalid characters and replace them with an underscore.
replaceInvalidChars :: String -> String
replaceInvalidChars = map (\x -> if x `elem` invalidChars then '_' else x)
  where
    invalidChars = "[]!} (){->,$'"
