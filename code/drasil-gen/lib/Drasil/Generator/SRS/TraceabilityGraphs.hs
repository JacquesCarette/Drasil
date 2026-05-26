{-# LANGUAGE QuasiQuotes #-}

module Drasil.Generator.SRS.TraceabilityGraphs (outputDot) where

import Prelude hiding ((<>))

import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), vcat, nest,
  hsep, empty)

import Drasil.Build.Artifacts (FileLayout, directory, file, ps)
import Drasil.Database (UID)
import Drasil.Metadata.TraceabilityGraphs (GraphInfo(..), NodeFamily(..),
  Label)

-- | Creates a `FileLayout`s for the generated TraceyGraph directory.
outputDot :: GraphInfo -> FileLayout Doc
outputDot gi =
  directory
  [ps|TraceyGraph|]
  [ mkOutput gi "avsa" edgesAvsA [assumpNF],
    mkOutput gi "avsall" edgesAvsAll [assumpNF, ddNF, tmNF, gdNF, imNF, reqNF, chgNF],
    mkOutput gi "refvsref" edgesRefvsRef [ddNF, tmNF, gdNF, imNF],
    mkOutput gi "allvsr" edgesAllvsR [assumpNF, ddNF, tmNF, gdNF, imNF, reqNF, gsNF],
    mkOutput gi "allvsall" edgesAllvsAll [assumpNF, ddNF, tmNF, gdNF, imNF, reqNF, gsNF, chgNF]
  ]

-- | General output function for making a traceability graph. Takes in the graph information, title, edge generator functions, and node family functions.
mkOutput :: GraphInfo -> String -> (GraphInfo -> [(UID, [UID])]) -> [GraphInfo -> NodeFamily] -> FileLayout Doc
mkOutput gi ttl getDirections getLabels =
  file [ps|{ttl}.dot|] (mkDot ttl (getDirections gi) (map ($ gi) getLabels))

-- | Constructs the full DOT document.
mkDot :: String -> [(UID, [UID])] -> [NodeFamily] -> Doc
mkDot title edges families =
  vcat
    [ text "digraph" <+> quote title <+> text "{",
      nest 4 $ vcat $ map vcat [map mkDirections edges, map mkNodes families],
      text "}"
    ]

mkDirections :: (UID, [UID]) -> Doc
mkDirections (u, deps) = vcat $ map (mkEdge u) (filter (not . null . show) deps)
  where
    mkEdge src dest = quote (show src) <+> text "->" <+> quote (show dest) <> text ";"

mkNodes :: NodeFamily -> Doc
mkNodes NF {nodeUIDs = u, nodeLabels = ls, nfLabel = lbl, nfColour = col} =
  vcat
    [ vcat (zipWith (mkNode col) u ls),
      mkSubgraph lbl u
    ]
  where
    mkNode c n l = quote (show n)
      <+> text "[shape=box, color=black, style=filled, fillcolor="
      <> quote c <> text ", label=" <> quote l <> text "];"

mkSubgraph :: Label -> [UID] -> Doc
mkSubgraph title contents
  | null title || null contents = empty
  | otherwise =
      vcat
        [ text "subgraph" <+> quote title <+> text "{",
          nest 4 $ vcat
            [ text "rank=\"same\";",
              hsep (map (quote . show) contents) <> text ";"
            ],
          text "}"
        ]

quote :: String -> Doc
quote = text . escape
  where
    escape s = "\"" ++ concatMap escChar s ++ "\""
    escChar '"' = "\\\""
    escChar '\\' = "\\\\"
    escChar c = [c]
