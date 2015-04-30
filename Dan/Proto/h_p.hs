module H_p where
import Chunk
import Text.PrettyPrint

h_p :: Chunk FName FDesc
h_p = newChunk $
  [("Symbol",text "$h_p$"),
   ("Description", text "initial gap film conductance")
  ]