module Tau_c where
import Chunk

tau_c :: Chunk FName FDesc
tau_c = newChunk $
  [("Symbol","$\\tau_{c}$"),
   ("Equation","\\tau_{c}"),
   ("Description","clad thickness")
  ]