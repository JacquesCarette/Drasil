module Language.Drasil.Code.Imperative.WriteReadMe (
  makeReadMe
) where 

import Language.Drasil.Mod (Name, Version)
import Language.Drasil.Choices (ImplementationType(..))
import Language.Drasil.Printers (makeMd, introInfo, sumInfo, invalidOS, contSep,
    instDoc, regularSec, filtEmp)

import Data.List (intersperse)
import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ (Doc, empty, isEmpty, vcat, text, doubleQuotes, 
    (<+>))


makeReadMe :: String -> String -> Maybe String -> ImplementationType -> 
    [(Name,Version)] -> String -> Doc
makeReadMe progLang progLangVers unsupportedOSs imp extLibs caseName = 
    makeMd [introInfo caseName,
    sumInfo caseName progLang progLangVers,
    dependencies extLibs,
    invalidOS unsupportedOSs,
    makeInstr imp]

dependencies:: [(Name, Version)] -> Doc
dependencies lib = 
    let formattedLibs = (vcat . intersperse contSep . filtEmp . 
            map libStatment) lib
    in if isEmpty formattedLibs then empty else 
            regularSec (text "Program Dependencies") formattedLibs

makeInstr :: ImplementationType -> Doc
makeInstr Library = empty
makeInstr Program = instDoc

-- Helper for dependencies
libStatment :: (Name, Version) -> Doc
libStatment ("","") = empty
libStatment (nam,vers) = (doubleQuotes . text) nam <+> text "version" <+> text vers