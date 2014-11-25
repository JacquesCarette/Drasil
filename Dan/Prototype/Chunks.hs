module Chunks where

-------------------------------------------------------------------------------
-- Chunks!
-------------------------------------------------------------------------------
import InternalAST

data Chunk = Chunk Ref [Definition]
  deriving (Eq, Show, Read)
type Ref = String
refHeader = "Name" 
terms = refHeader : "Data" : [] 
  
validChunk (Chunk _ d) = contains terms d

contains (t:ts) d = (contains ts d) && (any ((==t) . fst) $ d)
contains [] _ = True

makeChunk d = Chunk (createRef d) d

createRef :: [Definition] -> Ref
createRef (d:ds) = if ((fst d) == refHeader)
                  then snd d 
                  else createRef ds
createRef [] = error "Definition list missing the reference header"

hasRef (Chunk ref _) = \r -> r == ref
--parseChunk TODO
