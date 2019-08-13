module Language.Drasil.Code.Imperative.Input (
  makeInputFile
) where 

import Database.Drasil (ChunkDB)
import Language.Drasil
import Language.Drasil.Code.DataDesc (DataDesc, Data(..), LinePattern(..))
import Language.Drasil.Code.Imperative.GOOL.Helpers (blank)
import Language.Drasil.Printers (Linearity(Linear), sentenceDoc)

import Control.Lens ((^.), view)
import Data.List (intersperse)
import Text.PrettyPrint.HughesPJ (Doc, (<+>), char, empty, hcat, parens, text, 
  vcat)

makeInputFile :: ChunkDB -> DataDesc -> Doc
makeInputFile db dd = vcat (convDataDesc dd)
  where convDataDesc (JunkData : dds@(Singleton d : _)) = text "#" <+> 
          sentenceDoc db Implementation Linear (phraseNP $ d ^. term) <+> 
          maybe empty (parens . sentenceDoc db Implementation Linear . phraseNP 
          . view term) (getUnit d) : convDataDesc dds
        convDataDesc (Singleton _ : ds) = text "placeholder" : convDataDesc ds
        convDataDesc (Line lp d : ds) = convLinePatt lp d : convDataDesc ds
        convDataDesc (Lines lp Nothing d : ds) = convLinePatt lp d : 
          convDataDesc ds
        convDataDesc (Lines lp (Just n) d : ds) = convLinePatt lp d : 
          convDataDesc (if n == 1 then ds else Lines lp (Just (n-1)) d : ds)
        convDataDesc (JunkData : ds) = blank : convDataDesc ds
        convDataDesc _ = []
        convLinePatt (Straight ds) d = hcat $ intersperse (char d) $ map (const $ text "placeholder") ds
        convLinePatt (Repeat ds) d = hcat $ intersperse (char d) $ map (const $ text "placeholder") ds