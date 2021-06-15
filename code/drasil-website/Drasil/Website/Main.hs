module Drasil.Website.Main where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, DocSpec(DocSpec), DocType(Website))
import Drasil.Website (srs, printSetting)

main :: IO()
main = do
    setLocaleEncoding utf8
    gen (DocSpec Website "Drasil_Website") srs printSetting