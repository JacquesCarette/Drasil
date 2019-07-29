module Language.Drasil.Code.Imperative.Input (
  makeInputFile
) where 

import Language.Drasil
import Language.Drasil.Code.DataDesc (DataDesc, Data(..))
import Language.Drasil.Code.Imperative.Helpers (getStrSimple)

import Control.Lens ((^.), view)
import Text.PrettyPrint.HughesPJ (Doc, (<+>), empty, parens, text, vcat)

makeInputFile :: DataDesc -> Doc
makeInputFile dd = vcat (convDataDesc dd)
  where convDataDesc (JunkData : Singleton d : ds) = [
          text "#" <+> text (getStrSimple $ phraseNP $ d ^. term) <+> 
          maybe empty (parens . text . getStrSimple . phraseNP . view term) 
          (getUnit d), text "placeholder"] 
          ++ convDataDesc ds
        convDataDesc _ = []