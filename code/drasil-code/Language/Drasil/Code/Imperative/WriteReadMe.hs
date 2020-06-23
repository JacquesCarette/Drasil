module Language.Drasil.Code.Imperative.WriteReadMe (
  makeReadMe
) where 

import Language.Drasil.Mod (Name, Version)
import Language.Drasil.Choices (ImplementationType(..))
import Language.Drasil.Printers (makeMd, introInfo, sumInfo, invalidOS, contSep,
    instDoc, regularSec, filtEmp)

import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ (Doc, empty, isEmpty, vcat, text, doubleQuotes, 
    (<+>), punctuate)


makeReadMe :: String -> String -> Maybe String -> ImplementationType -> 
    [(Name,Version)] -> [FilePath] -> [String] -> String -> Doc
makeReadMe progLang progLangVers unsupportedOSs imp extLibns extLibfp auths caseName= 
    makeMd [introInfo caseName auths,
    sumInfo caseName progLang progLangVers,
    dependencies extLibns extLibfp,
    invalidOS unsupportedOSs,
    makeInstr imp]

dependencies:: [(Name, Version)] -> [FilePath]-> Doc
dependencies libns libfps = 
    let libs = addListToTuple libns libfps
        formattedLibs = (vcat . punctuate contSep . filtEmp . 
            map libStatment) libs
    in if isEmpty formattedLibs then empty else 
            regularSec (text "Program Dependencies") formattedLibs

makeInstr :: ImplementationType -> Doc
makeInstr Library = empty
makeInstr Program = instDoc

-- Helper for dependencies
libStatment :: (Name, Version, FilePath) -> Doc
libStatment ("","", _) = empty
libStatment (nam,vers, fp) = (doubleQuotes . text) nam <+> text "version" 
    <+> text vers <+> if fp == "" then empty else
    text ", local file path to the library" <+> text fp

addListToTuple :: [(Name,Version)] -> [FilePath] -> [(Name, Version, FilePath)]
addListToTuple [] [] = []
addListToTuple ((n,v):_) [] = [(n,v,"")]
addListToTuple ((n,v):xtup) (l:xlst) = [(n,v,l)] ++ addListToTuple xtup xlst
addListToTuple _ _ = []