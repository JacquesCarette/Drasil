module Language.Drasil.Template.MG(makeMG) where

import Language.Drasil.Document
import Language.Drasil.Chunk
import Language.Drasil.Chunk.Module
import Language.Drasil.Spec
import Language.Drasil.Printing.Helpers
import Language.Drasil.Reference
import Language.Drasil.Template.Helpers

import Control.Lens ((^.))
import Data.List (nub)
import Data.Maybe (fromJust, isNothing)


makeMG :: [ModuleChunk] -> ([Section], [Contents])
makeMG mcs = let mhier  = buildMH $ splitLevels mcs
                 mpairs = map createMPair (getMHOrder mhier)
                 hierTable = mgHierarchy $ formatMH mhier
  in ( [ mgModuleHierarchy mpairs hierTable,
         mgModuleDecomp mpairs ],
       getMods mpairs )


mgModuleHierarchy :: [MPair] -> Contents -> Section
mgModuleHierarchy mpairs hierTable =
  Section 0 (S "Module Hierarchy") (
    [ Con $ mgModuleHierarchyIntro hierTable ]
    ++ (map Con $ getMods mpairs)
    ++ [Con hierTable]
  )

mgModuleHierarchyIntro :: Contents -> Contents
mgModuleHierarchyIntro t@(Table _ _ _ _) = Paragraph $
  S "This section provides an overview of the module design. Modules are " :+:
  S "summarized in a hierarchy decomposed by secrets in " :+:
  makeRef t :+:
  S ". The modules listed below, which are leaves in the hierarchy tree, " :+:
  S "are the modules that will actually be implemented."


mgHierarchy :: [[Sentence]] -> Contents
mgHierarchy mh = let cnt = length $ head mh
                     hdr = map (\x -> S $ "Level " ++ show x) $ take cnt [1..]
                 in  Table hdr mh (S "Module Hierarchy") True

mgModuleDecomp :: [MPair] -> Section
mgModuleDecomp mpairs = let levels = splitLevels $ getChunks mpairs
  in Section 0 (S "Module Decomposition") (
       [Con $ mgModuleDecompIntro $ getChunks mpairs]
       ++ map (\x -> Sub (mgModuleInfo x levels)) mpairs
     )

mgModuleDecompIntro :: [ModuleChunk] -> Contents
mgModuleDecompIntro mcs =
  let impl ccs = foldl1 (:+:) $ map (\x -> (S $ "If the entry is " ++
       (x ^. name) ++ ", this means that the module is provided by the ")
       :+: (x ^. descr) :+: S ". ") ccs
  in Paragraph $
    S "Modules are decomposed according to the principle of \"information " :+:
    S "hiding\" proposed by Parnas. The Secrets field in a module " :+:
    S "decomposition is a brief statement of the design decision hidden by " :+:
    S "the module. The Services field specifies what the module will do " :+:
    S "without documenting how to do it. For each module, a suggestion for " :+:
    S "the implementing software is given under the Implemented By title. " :+:
    impl (nub $ getImps mcs) :+:
    S "Only the leaf modules in the hierarchy have to be implemented. If a " :+:
    S "dash (--) is shown, this means that the module is not a leaf and " :+:
    S "will not have to be implemented. Whether or not this module is " :+:
    S "implemented depends on the programming language selected."
      where
        getImps []     = []
        getImps (m:ms) = if imp m == Nothing
                         then getImps ms
                         else (fromJust $ imp m):getImps ms

mgModuleInfo :: MPair -> [[ModuleChunk]] -> Section
mgModuleInfo (mc, m) ls = let title = if   isNothing m
                                   then S (formatName mc)
                                   else S (formatName mc) :+: S " (" :+:
                                          (makeRef $ fromJust m) :+: S ")"
                              level = getLevel mc ls
  in Section (1 + level)
    title
    [ Con $ Enumeration $ Desc
      [(S "Secrets", Flat (secret mc)),
       (S "Services", Flat (mc ^. descr)),
       (S "Implemented By", Flat (getImp $ imp mc))
      ]
    ]
    where
      getImp (Just x) = S (x ^. name)
      getImp _        = S "--"