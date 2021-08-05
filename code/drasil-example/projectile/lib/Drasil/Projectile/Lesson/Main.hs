module Drasil.Projectile.Lesson.Main (main) where

import GHC.IO.Encoding

import Language.Drasil.Generate (gen, DocSpec(DocSpec), DocType(Jupyter), DebugOption(..), docChoices)

import Drasil.Projectile.Lesson.Body (nb, printSetting)

main :: IO()
main = do
  setLocaleEncoding utf8
  gen (DocSpec (docChoices Jupyter [] NoDebug) "Projectile Motion Lesson") nb printSetting
