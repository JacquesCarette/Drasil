module Tau_c where
import ASTInternal
import Chunk
import Config

tau_c :: Chunk FName FDesc
tau_c = newChunk $
  [(Symbol,G Tau_L :- S "{c}"),
   (Equation,G Tau_L :- S "{c}"),
   (Description,S "clad thickness")
  ]