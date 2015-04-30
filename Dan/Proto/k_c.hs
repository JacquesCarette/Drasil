module K_c where
import Chunk
import Text.PrettyPrint

k_c :: Chunk FName FDesc
k_c = newChunk $
  [("Symbol",text "$k_c$"),
   ("Description", text "clad conductivity")
  ]