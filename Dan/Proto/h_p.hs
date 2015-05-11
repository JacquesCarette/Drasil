module H_p where
import Chunk
import Config

h_p :: Chunk FName FDesc
h_p = newChunk $
  [(Symbol,"$h_{p}$"),
   (Equation, "h_{p}"),
   (Description, "initial gap film conductance")
  ]