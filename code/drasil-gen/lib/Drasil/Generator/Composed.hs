-- | Defines Drasil generator functions.
module Drasil.Generator.Composed (
  -- * Generators
  exportSmithEtAlSrsWCode, exportSmithEtAlSrsWCodeZoo,
) where

import Language.Drasil.Code (Choices)
import Drasil.SRSDocument (SRSDecl)
import Drasil.System (System)

import Drasil.Generator.Code (exportCode, exportCodeZoo)
import Drasil.Generator.SRS (exportSmithEtAlSrs)

-- | Generate an SRS softifact with a specific solution softifact.
exportSmithEtAlSrsWCode :: System -> SRSDecl -> String -> Choices -> IO ()
exportSmithEtAlSrsWCode syst srsDecl srsFileName chcs = do
  exportSmithEtAlSrs syst srsDecl srsFileName
  exportCode syst chcs

-- | Generate an SRS softifact with a zoo of solution softifacts.
exportSmithEtAlSrsWCodeZoo :: System -> SRSDecl -> String -> [Choices] -> IO ()
exportSmithEtAlSrsWCodeZoo syst srsDecl srsFileName chcs = do
  exportSmithEtAlSrs syst srsDecl srsFileName
  exportCodeZoo syst chcs
