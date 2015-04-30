module H_b where
import Chunk
import Text.PrettyPrint

h_b :: Chunk FName FDesc
h_b = newChunk $
  [("Symbol",text "$h_p$"),
   ("Description", text "initial coolant film conductance")
  ]