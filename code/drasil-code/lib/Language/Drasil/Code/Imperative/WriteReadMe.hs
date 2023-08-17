module Language.Drasil.Code.Imperative.WriteReadMe (
  makeReadMe
) where

import Language.Drasil.Choices (ImplementationType(..))
import Language.Drasil.Printers (makeMd, introInfo, verInfo, unsupOS,
    extLibSec, instDoc, endNote, whatInfo)
import Language.Drasil.Code.Imperative.GOOL.ClassInterface (ReadMeInfo(..))

import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ (Doc, empty)
import Data.List.NonEmpty (nonEmpty, toList)

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
        caseName = name,
        examplePurpose = purp,
        exampleDescr = descr,
        folderNum = number,
        inputOutput = inoutf} =
    makeMd [introInfo name auths $ fieldEmptySTR purp,
    whatInfo $ fieldEmptySTR descr,
    makeInstr imptype configFPs name inoutf,
    verInfo progLang progLangVers,
    unsupOS unsupportedOSs,
    extLibSec extLibns extLibfp,
    endNote number auths] -- add date information to end note for license

-- | Helper for encoding the type of program (either library or controller-based) in a README file.
makeInstr :: ImplementationType -> [FilePath] -> String -> (String, String) -> Doc
makeInstr Library _ _ _ = empty
makeInstr Program cfp n inOutf = instDoc cfp n inOutf

-- |Helper that checks if the field is empty; allowing optional content
-- rendering in a README file
fieldEmptySTR :: String -> Maybe String
fieldEmptySTR = fmap toList . nonEmpty
