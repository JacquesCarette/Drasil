module K_c where
import Chunk

k_c :: Chunk FName FDesc
k_c = newChunk $
  [("Symbol","$k_{c}$"),
   ("Equation", "k_{c}"),
   ("Description", "clad conductivity")
  ]