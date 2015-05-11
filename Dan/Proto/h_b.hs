module H_b where
import Chunk
import Config

h_b :: Chunk FName FDesc
h_b = newChunk $
  [(Symbol,"$h_{b}$"),
   (Equation,"h_{b}"),
   (Description, "initial coolant film conductance")
  ]