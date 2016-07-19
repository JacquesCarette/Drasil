module Language.Drasil.Template where

import Language.Drasil.Document
import Language.Drasil.Chunk
import Language.Drasil.Spec

import Control.Lens ((^.))

mgModuleDecomp :: [ConceptChunk] -> LayoutObj
mgModuleDecomp ccs =
  let impl = foldl1 (:+:) $ map (\x -> (S $ "If the entry is " ++ (x ^. name) ++
        ", this means that the module is provided by the ") :+: (x ^. descr) :+:
        S ". ") ccs
  in
    Section 0 (S "Module Decomposition") [Paragraph $
    S "Modules are decomposed according to the principle of \"information " :+:
    S "hiding\" proposed by Parnas. The Secrets field in a module " :+:
    S "decomposition is a brief statement of the design decision hidden by " :+:
    S "the module. The Services field specifies what the module will do " :+:
    S "without documenting how to do it. For each module, a suggestion for " :+:
    S "the implementing software is given under the Implemented By title. " :+:
    impl :+:
    S "Only the leaf modules in the hierarchy have to be implemented. If a " :+:
    S "dash (--) is shown, this means that the module is not a leaf and " :+:
    S "will not have to be implemented. Whether or not this module is " :+:
    S "implemented depends on the programming language selected."]