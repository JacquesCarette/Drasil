module Language.Drasil.Code.Imperative.WriteReadMe (
  makeReadMe
) where 

import Language.Drasil.Choices (ImplementationType(..))
import Language.Drasil.Printers (makeMd, introInfo, verInfo, unsupOS, 
    extLibSec, instDoc, endNote)
import Language.Drasil.Code.Imperative.GOOL.ClassInterface (ReadMeInfo(..))

import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ (Doc, empty)

-- | Generates a README file.
makeReadMe :: ReadMeInfo -> Doc 
makeReadMe ReadMeInfo {
        langName = progLang,
        langVersion = progLangVers,
        invalidOS = unsupportedOSs,
        implementType = imptype,
        extLibNV = extLibns,
        extLibFP = extLibfp,
        contributors = auths, 
        configFP = configFPs,
        caseName = name} = 
    makeMd [introInfo name auths,
    makeInstr imptype configFPs,
    verInfo progLang progLangVers,
    unsupOS unsupportedOSs,
    extLibSec extLibns extLibfp,
    endNote auths] -- add date information to end note for license

-- | Helper for encoding the type of program (either library or controller-based) in a README file.
makeInstr :: ImplementationType -> [FilePath]-> Doc
makeInstr Library _ = empty
makeInstr Program cfp = instDoc cfp
