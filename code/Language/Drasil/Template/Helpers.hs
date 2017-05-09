module Language.Drasil.Template.Helpers where

import Language.Drasil.Document
import Language.Drasil.Chunk.Module
import Language.Drasil.Spec
import Language.Drasil.Unicode
import Language.Drasil.Reference

import Data.List ((\\), intersperse)
import Data.Maybe (fromJust, isNothing)
import Data.List.Split (splitOn)

type MPair = (ModuleChunk, Maybe Contents)

createMPair :: ModuleChunk -> MPair
createMPair mc = if   (imp mc == Nothing)
                 then (mc, Nothing)
                 else (mc, Just $ Module mc)

getChunks :: [MPair] -> [ModuleChunk]
getChunks [] = []
getChunks ((mc,_):mps) = mc:getChunks mps

getMods :: [MPair] -> [Contents]
getMods [] = []
getMods ((_,Nothing):mps) = getMods mps
getMods ((_,Just m):mps)  = m:getMods mps

--findMod :: ModuleChunk -> [MPair] -> Maybe Contents
--findMod _ [] = Nothing
--findMod mca ((mcb,m):mps) = if mca == mcb then m else findMod mca mps

buildMH :: [[ModuleChunk]] -> [[Maybe ModuleChunk]]
buildMH mcl = map (padBack (length mcl)) $ buildMH' Nothing mcl
  where buildMH' _ []                 = []
        buildMH' _ ([]:_)             = []
        buildMH' mLast ((mc:mcs):mcl') =
          let nextCol = buildMH' (Just mc) mcl'
              padCnt = length nextCol - 1
          in  if (hier mc == mLast)
              then ((Just mc):replicate padCnt (Nothing)) `jCols` nextCol
                ++ buildMH' mLast (mcs:mcl')
              else buildMH' mLast (mcs:mcl')
          where jCols [] []         = []
                jCols x []          = [x]
                jCols (x:xs) (y:ys) = [x:y] ++ jCols xs ys
                jCols _ _           = error "Unexpected pattern"
        padBack n s = s ++ replicate (n - length s) (Nothing)

formatMH :: [[Maybe ModuleChunk]] -> [[Sentence]]
formatMH = map (map (\x -> if   isNothing x
                           then (EmptyS)
                           else (S $ formatName $ fromJust x)))

getMHOrder :: [[Maybe ModuleChunk]] -> [ModuleChunk]
getMHOrder = concat . map removeNothing
  where removeNothing []       = []
        removeNothing (mc:mcs) = if isNothing mc
                                 then removeNothing mcs
                                 else (fromJust mc):removeNothing mcs

splitLevels :: [ModuleChunk] -> [[ModuleChunk]]
splitLevels mcs = splitLevels' mcs []
  where splitLevels' [] sl  = sl
        splitLevels' mcs' [] = let level = splitLevelsInit mcs'
                      in  splitLevels' (mcs' \\ level) [level]
          where splitLevelsInit []         = []
                splitLevelsInit (mc:mcs'') = if   (hier mc == Nothing)
                                             then mc:splitLevelsInit mcs''
                                             else splitLevelsInit mcs''
        splitLevels' mcs' sl = let level = splitLevels'' (last sl) mcs'
                               in  splitLevels' (mcs' \\ level) (sl ++ [level])
          where splitLevels'' _ []          = []
                splitLevels'' prev (mc:mcs'') = let current = fromJust $ hier mc
                                                in  if   current `elem` prev
                                                    then mc:splitLevels'' prev mcs''
                                                    else splitLevels'' prev mcs''

getLevel :: ModuleChunk -> [[ModuleChunk]] -> Int
getLevel mc ls = getLevel' mc ls 0
  where getLevel' _ [] _      = error "Module not found"
        getLevel' mc' (l:ls') n = if   (mc' `elem` l)
                                  then min n 2
                                  else getLevel' mc' ls' (n+1)


getMISModules :: [Contents] -> [ModuleChunk]
getMISModules [] = []
getMISModules ((Module mc):ms) = if   null $ method mc
                                 then getMISModules ms
                                 else mc:getMISModules ms
getMISModules _                = error "Wrong content type"

convertName :: String -> Sentence
convertName n = foldl1 (:+:) $ intersperse (Sp UScore) $
  map (S) (splitOn "_" n)


--createLCPair :: LCChunk -> LCPair
--createLCPair lcc = (lcc, )

docOutline :: [(Section, Sentence)] -> Contents
docOutline secDesc = Paragraph $ foldl1 (:+:) $ intersperse (S " ") $
  map (\(sec, desc) -> (makeRef sec) :+: S " " :+: desc) secDesc