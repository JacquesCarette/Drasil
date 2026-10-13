-- | Defines requirements shared by many examples.
module Data.Drasil.Requirements (portable) where

import Language.Drasil
import Language.Drasil.Document (ConceptInstance, cic)

import Drasil.Metadata.Concepts.Computation (defaultOSs)
import Drasil.Metadata.Documentation (nonFuncReqDom)

-- | Common Non-Functional Requirement for Portability, targeting 'defaultOSs'.
portable :: ConceptInstance
portable = cic "portable" (foldlSent [
  S "The code shall be portable to multiple environments, particularly",
  foldlList Comma List $ phrase <$> defaultOSs
  ]) "Portability" nonFuncReqDom
