module H_p where
import ASTInternal
import Chunk
import Config

h_p :: Chunk FName FDesc
h_p = newChunk $
  [(Symbol, S "h":- S "{p}"),
   (Equation, S "h":- S "{p}"),
   (Description, S "initial gap film conductance")
  ]