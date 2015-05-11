module K_c where
import ASTInternal
import Chunk
import Config

k_c :: Chunk FName FDesc
k_c = newChunk $
  [(Symbol,S "k":- S "{c}"),
   (Equation, S "k":- S "{c}"),
   (Description, S "clad conductivity")
  ]