module Language.Drasil.Template where

import Language.Drasil.Document
import Language.Drasil.Chunk
import Language.Drasil.Chunk.Module
import Language.Drasil.Spec
import Language.Drasil.Printing.Helpers

import Control.Lens ((^.))
import Data.List (nub, (\\))
import Data.Maybe (fromJust)

mgModuleHierarchy :: [ModuleChunk] -> Section
mgModuleHierarchy mcs =
  Section 0 (S "Module Hierarchy") (
    [Con $ mgModuleHierarchyIntro]
    ++ map (Con . Module) mcs
    ++ [Con $ mgHierarchy mcs]
  )

mgModuleHierarchyIntro :: Contents
mgModuleHierarchyIntro = Paragraph $
  S "This section provides an overview of the module design. Modules are " :+:
  S "summarized in a hierarchy decomposed by secrets in Table (todo). The " :+:
  S "modules listed below, which are leaves in the hierarchy tree, are the " :+:
  S "modules that will actually be implemented."


mgHierarchy :: [ModuleChunk] -> Contents
mgHierarchy mcs = let mh = buildMH $ splitLevels mcs []
                      cnt = length $ head mh
                      hdr = map (\x -> S $ "Level " ++ show x) $ take cnt [1..]
                  in Table hdr mh (S "") False

buildMH :: [[ModuleChunk]] -> [[Sentence]]
buildMH mcl = map (padBack (length mcl)) $ buildMH' Nothing mcl
  where buildMH' _ []             = []
        buildMH' _ ([]:_)         = []
        buildMH' mLast ((mc:mcs):mcl) =
          let nextCol = buildMH' (Just mc) mcl
              padCnt = length nextCol - 1
          in  if (hier mc == mLast)
              then ((S $ formatName mc):replicate padCnt (S "")) `jCols` nextCol
                ++ buildMH' mLast (mcs:mcl)
              else buildMH' mLast (mcs:mcl)
        padBack :: Int -> [Sentence] -> [Sentence]
        padBack n s = s ++ replicate (n - length s) (S "")

jCols :: [a] -> [[a]] -> [[a]]
jCols [] [] = []
jCols x []  = [x]
jCols (x:xs) (y:ys) = [x:y] ++ jCols xs ys


splitLevels :: [ModuleChunk] -> [[ModuleChunk]] -> [[ModuleChunk]]
splitLevels [] sl  = sl
splitLevels mcs [] = let level = splitLevelsInit mcs
                     in  splitLevels (mcs \\ level) [level]
  where splitLevelsInit []         = []
        splitLevelsInit (mc:mcs)   = if   (hier mc == Nothing)
                                     then mc:splitLevelsInit mcs
                                     else splitLevelsInit mcs
splitLevels mcs sl = let level = splitLevels' (last sl) mcs
                     in  splitLevels (mcs \\ level) (sl ++ [level])
  where splitLevels' _ []          = []
        splitLevels' prev (mc:mcs) = let current = fromJust $ hier mc
                                     in  if   current `elem` prev
                                         then mc:splitLevels' prev mcs
                                         else splitLevels' prev mcs

mgModuleDecomp :: [ModuleChunk] -> Section
mgModuleDecomp mcs =
  Section 0 (S "Module Decomposition") (
    [Con $ mgModuleDecompIntro mcs]
    ++ map (Sub . mgModuleInfo) mcs
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

mgModuleInfo :: ModuleChunk -> Section
mgModuleInfo mc = Section 1
  ( S $ (formatName mc) )
  [ Con $ Enumeration $ Desc
    [(S "Secrets", Flat (secret mc)),
     (S "Services", Flat (mc ^. descr)),
     (S "Implemented By", Flat (getImp $ imp mc))
    ]
  ]
  where
    getImp (Just x) = S (x ^. name)
    getImp _        = S "--"