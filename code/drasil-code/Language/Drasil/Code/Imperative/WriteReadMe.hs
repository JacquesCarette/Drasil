module Language.Drasil.Code.Imperative.WriteReadMe (
  makeReadMe
) where 
import Text.PrettyPrint.HughesPJ (Doc, (<+>), char, empty, hcat, parens, space, 
  text, vcat)

makeReadMe :: String -> Doc
makeReadMe = text