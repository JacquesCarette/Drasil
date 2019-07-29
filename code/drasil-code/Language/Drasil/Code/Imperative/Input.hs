module Language.Drasil.Code.Imperative.Input (
  makeInputFile
) where 

import Database.Drasil (ChunkDB)
import Language.Drasil
import Language.Drasil.Code.DataDesc (DataDesc, Data(..))
import Language.Drasil.Code.Imperative.Helpers (getStr)

import Control.Lens ((^.), view)
import Text.PrettyPrint.HughesPJ (Doc, (<+>), empty, parens, text, vcat)

makeInputFile :: ChunkDB -> DataDesc -> Doc
makeInputFile db dd = vcat (convDataDesc dd)
  where convDataDesc (JunkData : Singleton d : ds) = [
          text "#" <+> text (getStr db $ phraseNP $ d ^. term) <+> 
          maybe empty (parens . text . getStr db . phraseNP . view term) 
          (getUnit d), text "placeholder"] 
          ++ convDataDesc ds
        convDataDesc _ = []