module Language.Drasil.Plain.Print (
  sentenceDoc, symbolDoc, toPlainName
) where

import Database.Drasil(ChunkDB, termTable)
import Language.Drasil
import Language.Drasil.Plain.Helpers (toPlainName)

import Prelude hiding ((<>))
import Control.Lens (view)
import qualified Data.Map as Map (lookup)
import Text.PrettyPrint.HughesPJ (Doc, (<>), empty, text)

sentenceDoc :: ChunkDB -> Sentence -> Doc
sentenceDoc _ (S s) = text s
sentenceDoc _ (P s) = symbolDoc s
sentenceDoc db ((:+:) s1 s2) = sentenceDoc db s1 <> sentenceDoc db s2
sentenceDoc db (Ch _ u) = maybe empty (sentenceDoc db . phraseNP . view term . 
  fst) (Map.lookup u (termTable db))
sentenceDoc _ _ = error "Term is not a string" 

symbolDoc :: Symbol -> Doc
symbolDoc (Atomic s) = text $ toPlainName s
symbolDoc (Special sp) = specialDoc sp
symbolDoc (Atop d s) = decorate (symbolDoc s) d
symbolDoc (Corners ul ll ur lr b) =
  cleft ul <> cleft ll <> symbolDoc b <> cright lr <> cright ur
  where cleft :: [Symbol] -> Doc
        cleft [] = empty
        cleft (s:syms) = symbolDoc s <> text "_" <> cleft syms
        cright :: [Symbol] -> Doc
        cright [] = empty
        cright (s:syms) = text "_" <> symbolDoc s <> cright syms
symbolDoc (Concat s) = foldl1 (<>) $ map symbolDoc s
symbolDoc Empty = empty

decorate :: Doc -> Decoration -> Doc
decorate s Hat = s <> text "_hat"
decorate s Vector = s <> text "_vect"
decorate s Prime = s <> text "'"

-- TODO: Double check that this is valid in all output languages
specialDoc :: Special -> Doc
specialDoc Circle  = text "circ"
specialDoc Partial = text "partial"