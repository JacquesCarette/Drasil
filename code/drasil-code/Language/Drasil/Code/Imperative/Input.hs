module Language.Drasil.Code.Imperative.Input (
  makeInputFile, readWithDataDesc, sampleInputDD
) where 

import Database.Drasil (ChunkDB)
import Language.Drasil
import qualified Language.Drasil.Code.Code as C (CodeType(..))
import Language.Drasil.Code.DataDesc (DataDesc, Data(..), DataItem, Delim,
  LinePattern(..), getDataInputs, isJunk, junkLine, repeated, singleton, 
  singleLine)
import Language.Drasil.Code.Imperative.GOOL.Helpers (blank)
import Language.Drasil.Chunk.Code (codeType)
import Language.Drasil.Printers (Linearity(Linear), exprDoc, sentenceDoc, 
  unitDoc)

import Control.Lens (view)
import Data.List (intersperse, transpose)
import Data.List.Split (wordsBy)
import Text.PrettyPrint.HughesPJ (Doc, (<+>), char, empty, hcat, parens, space, 
  text, vcat)

makeInputFile :: ChunkDB -> DataDesc -> [Expr] -> Doc
makeInputFile db dd sampData = vcat (convDataDesc dd sampData)
  where convDataDesc [] (_:_) = error $ "makeInputFile received more inputs" ++ 
          " than expected, should be impossible"
        convDataDesc ds [] = if all isJunk ds then replicate (length ds) blank 
          else error "makeInputFile received fewer inputs than expected, should be impossible"
        convDataDesc (JunkData : ds@(Singleton _ : _)) es = docLine ds ' ' es
        convDataDesc (JunkData : ds@(Line _ dl : _)) es = docLine ds dl es
        convDataDesc (JunkData : ds@(Lines _ _ dl : _)) es = docLine ds dl es
        convDataDesc (Singleton _ : ds) (e:es) = eDoc e : convDataDesc ds es
        convDataDesc (Line (Straight dis) dl : ds) es = let 
          (l,ls) = splitAt (length dis) es 
          in dataLine dl l : convDataDesc ds ls
        convDataDesc (Line (Repeat dis) dl : ds) es = let 
          (l,ls) = splitAt (length dis) es 
          in dataLine dl (concat $ orderVecs l)
          : convDataDesc ds ls
        convDataDesc (Lines (Straight _) Nothing dl : _) es = map (dataLine dl) 
          (orderVecs es)
        convDataDesc (Lines (Straight dis) (Just n) dl : ds) es = let
          (l,ls) = splitAt (length dis) es 
          vs = orderVecs l
          in if toInteger (length vs) == n then map (dataLine dl) vs
          ++ convDataDesc ds ls
          else error "makeInputFile encountered wrong-sized vectors"
        convDataDesc (Lines (Repeat _) Nothing dl : _) es = map 
          (dataLine dl . concat . transpose) (orderMtxs es)
        convDataDesc (Lines (Repeat dis) (Just n) dl : ds) es = let
          (l,ls) = splitAt (length dis) es
          ms = orderMtxs l
          in if toInteger (length ms) == n then map
          (dataLine dl . concat . transpose) (transpose ms) 
          ++ convDataDesc ds ls
          else error "makeInputFile encountered wrong-sized matrices"
        convDataDesc (JunkData : ds) es = blank : convDataDesc ds es
        sDoc = sentenceDoc db Implementation Linear
        eDoc = exprDoc db Implementation Linear
        uDoc = unitDoc Linear
        dataLine dl = hcat . intersperse (char dl) . map eDoc
        docLine ds dl es = let dis = getDataInputs (head ds) 
          in text "#" <+> hcat (intersperse (char dl <> space) 
          (map (\di -> (sDoc . phraseNP . view term) di <+> 
          maybe empty (parens . uDoc . usymb) (getUnit di)) dis)) 
          : convDataDesc ds es
        orderVecs vs = transpose $ map getVecList vs
        getVecList (Matrix [l]) = l
        getVecList _ = error "makeInputFile encountered unexpected type, expected vector"
        orderMtxs ms = transpose $ map getMtxLists ms
        getMtxLists (Matrix l) = l
        getMtxLists _ = error "makeInputFile encountered unexpected type, expected matrix"


readWithDataDesc :: FilePath -> DataDesc -> IO [Expr]
readWithDataDesc fp dd = do 
  ins <- readFile fp
  let readDD :: DataDesc -> [String] -> [Expr]
      readDD [] (_:_) = error "extra lines encountered in sample input file"
      readDD ds [] = if all isJunk ds then [] 
        else error "some inputs missing in sample input file"
      readDD (JunkData : ds) (_:ls) = readDD ds ls
      readDD (Singleton d : ds) (l:ls) = strAsExpr (codeType d) l : readDD ds ls
      readDD (Line (Straight dis) dl : ds) (l:ls) = zipWith strAsExpr (map 
        codeType dis) (wordsBy (==dl) l) ++ readDD ds ls
      readDD (Line (Repeat dis) dl : ds) (l:ls) = strListsAsExprs dis 
        (groupLists dis dl l) ++ readDD ds ls
      readDD (Lines (Straight dis) (Just n) dl : ds) ls = strListsAsExprs dis 
        (transpose (map (wordsBy (==dl)) (take (fromIntegral n) ls))) 
        ++ readDD ds (drop (fromIntegral n) ls)
      readDD (Lines (Straight dis) Nothing dl : _) ls = strListsAsExprs dis 
        (transpose (map (wordsBy (==dl)) ls))
      readDD (Lines (Repeat dis) (Just n) dl : ds) ls = strLists2DAsExprs dis 
        (map (groupLists dis dl) (take (fromIntegral n) ls)) 
        ++ readDD ds (drop (fromIntegral n) ls)
      readDD (Lines (Repeat dis) Nothing dl : _) ls = strLists2DAsExprs dis 
        (map (groupLists dis dl) ls)
  return $ readDD dd (lines ins)

sampleInputDD :: [DataItem] -> DataDesc
sampleInputDD cs = junkLine : intersperse junkLine (map dataDesc cs)
  where dataDesc c = toDataDesc (codeType c) c
        toDataDesc (C.List _) c = singleLine (repeated [c]) ','
        toDataDesc _ c = singleton c

-- helpers

strAsExpr :: C.CodeType -> String -> Expr
strAsExpr C.Integer s = int (read s :: Integer)
strAsExpr C.Float s = dbl (read s :: Double)
strAsExpr C.String s = str s
strAsExpr _ _ = error "strAsExpr should only be called on integers, floats, or strings"

strListsAsExprs :: [DataItem] -> [[String]] -> [Expr]
strListsAsExprs cs = zipWith ($) (map (strListAsExpr . codeType) cs)
  where strListAsExpr (C.List t) ss = Matrix [map (strAsExpr t) ss]
        strListAsExpr _ _ = error "strListsAsExpr called on non-list type"

strLists2DAsExprs :: [DataItem] -> [[[String]]] -> [Expr]
strLists2DAsExprs cs = zipWith ($) (map (strList2DAsExpr . codeType) cs)
  where strList2DAsExpr (C.List (C.List t)) sss = Matrix $ 
          map (map (strAsExpr t)) sss
        strList2DAsExpr _ _ = error "strLists2DAsExprs called on non-2D-list type"

groupLists :: [DataItem] -> Delim ->  String -> [[String]]
groupLists ds dl l = transpose $ groupAdjacent (wordsBy (==dl) l)
  where groupAdjacent [] = []
        groupAdjacent es = take n es : groupAdjacent (drop n es)
        n = length ds
