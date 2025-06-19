{-# LANGUAGE PostfixOperators #-}
module Drasil.SglPend.Assumptions (assumpSingle) where
    
import Language.Drasil (ConceptInstance)
import Drasil.DblPend.Assumptions (assumpBasic)

assumpSingle :: [ConceptInstance]
assumpSingle = assumpBasic
