module Tau_c where
import Chunk
import Text.PrettyPrint

tau_c :: Chunk FName FDesc
tau_c = newChunk $
  [("Symbol",text "$\\tau_c$"),
   ("Description", text "clad thickness")
  ]