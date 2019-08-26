module Language.Drasil.Code.Imperative.WriteInput (
  makeInputFile
) where 

import Database.Drasil (ChunkDB)
import Language.Drasil
import Language.Drasil.Code.DataDesc (DataDesc, Data(..), Delim, 
  LinePattern(..), getDataInputs, isJunk)
import Language.Drasil.Code.Imperative.GOOL.Helpers (blank)
import Language.Drasil.Printers (Linearity(Linear), exprDoc, sentenceDoc, 
  unitDoc)

import Control.Lens (view)
import Data.List (intersperse, transpose)
import Text.PrettyPrint.HughesPJ (Doc, (<+>), char, empty, hcat, parens, space, 
  text, vcat)

makeInputFile :: ChunkDB -> DataDesc -> [Expr] -> Doc
makeInputFile db dd sampData = vcat (convDataDesc db dd sampData)

convDataDesc :: ChunkDB -> DataDesc -> [Expr] -> [Doc]
convDataDesc _ [] (_:_) = error $ "makeInputFile received more inputs" ++ 
          " than expected, should be impossible"
convDataDesc _ ds [] = if all isJunk ds then replicate (length ds) blank 
  else error "makeInputFile received fewer inputs than expected, should be impossible"
convDataDesc db (JunkData : ds@(Singleton _ : _)) es = docLine db ds ' ' es
convDataDesc db (JunkData : ds@(Line _ dl : _)) es = docLine db ds dl es
convDataDesc db (JunkData : ds@(Lines _ _ dl : _)) es = docLine db ds dl es
convDataDesc db (Singleton _ : ds) (e:es) = eDoc db e : convDataDesc db ds es
convDataDesc db (Line (Straight dis) dl : ds) es = let 
  (l,ls) = splitAt (length dis) es 
  in dataLine db dl l : convDataDesc db ds ls
convDataDesc db (Line (Repeat dis) dl : ds) es = let 
  (l,ls) = splitAt (length dis) es 
  in dataLine db dl (concat $ orderVecs l)
  : convDataDesc db ds ls
convDataDesc db (Lines (Straight _) Nothing dl : _) es = map (dataLine db dl) 
  (orderVecs es)
convDataDesc db (Lines (Straight dis) (Just n) dl : ds) es = let
  (l,ls) = splitAt (length dis) es 
  vs = orderVecs l
  in if toInteger (length vs) == n then map (dataLine db dl) vs
  ++ convDataDesc db ds ls
  else error "makeInputFile encountered wrong-sized vectors"
convDataDesc db (Lines (Repeat _) Nothing dl : _) es = map 
  (dataLine db dl . concat . transpose) (orderMtxs es)
convDataDesc db (Lines (Repeat dis) (Just n) dl : ds) es = let
  (l,ls) = splitAt (length dis) es
  ms = orderMtxs l
  in if toInteger (length ms) == n then map
  (dataLine db dl . concat . transpose) ms
  ++ convDataDesc db ds ls
  else error "makeInputFile encountered wrong-sized matrices"
convDataDesc db (JunkData : ds) es = blank : convDataDesc db ds es

-- helpers 

dataLine :: ChunkDB -> Delim -> [Expr] -> Doc
dataLine db dl = hcat . intersperse (char dl) . map (eDoc db)

docLine :: ChunkDB -> DataDesc -> Delim -> [Expr] -> [Doc]
docLine db ds dl es = let dis = getDataInputs (head ds) 
  in text "#" <+> hcat (intersperse (char dl <> space) 
  (map (\di -> (sDoc db . phraseNP . view term) di <+> 
  maybe empty (parens . uDoc . usymb) (getUnit di)) dis)) 
  : convDataDesc db ds es

orderVecs :: [Expr] -> [[Expr]]
orderVecs vs = transpose $ map getVecList vs

getVecList :: Expr -> [Expr]
getVecList (Matrix [l]) = l
getVecList _ = error "makeInputFile encountered unexpected type, expected vector"

orderMtxs :: [Expr] -> [[[Expr]]]
orderMtxs ms = transpose $ map getMtxLists ms

getMtxLists :: Expr -> [[Expr]]
getMtxLists (Matrix l) = l
getMtxLists _ = error "makeInputFile encountered unexpected type, expected matrix"

sDoc :: ChunkDB -> Sentence -> Doc
sDoc db = sentenceDoc db Implementation Linear

eDoc :: ChunkDB -> Expr -> Doc
eDoc db = exprDoc db Implementation Linear

uDoc :: USymb -> Doc
uDoc = unitDoc Linear