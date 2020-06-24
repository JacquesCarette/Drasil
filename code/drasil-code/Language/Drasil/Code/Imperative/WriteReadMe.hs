module Language.Drasil.Code.Imperative.WriteReadMe (
  makeReadMe
) where 

import Language.Drasil.Mod (Name, Version)
import Language.Drasil.Choices (ImplementationType(..))
import Language.Drasil.Printers (makeMd, introInfo, verInfo, invalidOS, 
    extLibSec, instDoc)

import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ (Doc, empty)

makeReadMe :: String -> String -> Maybe String -> ImplementationType -> 
    [(Name,Version)] -> [FilePath] -> [String] -> [FilePath] -> String -> Doc
makeReadMe progLang progLangVers unsupportedOSs imptype extLibns extLibfp auths configFP caseName= 
    makeMd [introInfo caseName auths,
    makeInstr imptype configFP,
    verInfo progLang progLangVers,
    invalidOS unsupportedOSs,
    extLibSec extLibns extLibfp]

makeInstr :: ImplementationType -> [FilePath]-> Doc
makeInstr Library _ = empty
makeInstr Program cfp = instDoc cfp
