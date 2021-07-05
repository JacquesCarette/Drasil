module Language.Drasil.DOT.TraceabilityGraphGenerator where

{-- import needed 

import Language.Drasil.DOT.TraceabilityGraphGeneratorInfo as TGGI
import System.IO

type CaseStudy = String
type Chunk = String

-- input chunks to be included in generated traceability graph, outputs .dot file
-- e.g. sample usage: genTraceGraph SWHS [TMods, IMods, GenDefs]
genTraceGraph :: CaseStudy -> [Chunk] -> IO ()
genTraceGraph caseStudy [chunk] = do
	let nodeDefs = TGGI.getInfo ["all"]
	dotFile <- openFile (caseStudy ++ "_TG.dot") WriteMode

	hPutStrLn dotFile ("digraph " ++ caseStudy ++ "_TG {")
	hPutStrLn dotFile ("\tnewrank=true; rankdir=TB;\n")

	sequence_ (map (hPutStrLn dotFile) nodeDefs)

	hPutStrLn dotFile ("}")

	hClose dotFile-}
