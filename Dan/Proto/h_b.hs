{-# OPTIONS -Wall #-} 
module H_b where
import ASTInternal
import Chunk
import Config

h_b :: Chunk FName FDesc
h_b = newChunk $
  [(Symbol,S "h" :- S "{b}"),
   (Equation,S "h" :- S "{b}"),
   (Description, S "initial coolant film conductance")
  ]