module H_b where
import Chunk
import Text.PrettyPrint

h_b :: Chunk FName FDesc
h_b = newChunk $
  [("Symbol",text "$h_b$"),
   ("Equation",text "h_{b}"),
   ("Description", text "initial coolant film conductance")
  ]