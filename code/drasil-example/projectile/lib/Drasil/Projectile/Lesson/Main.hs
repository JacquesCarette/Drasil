module Drasil.Projectile.Lesson.Main (main) where

import GHC.IO.Encoding

import Language.Drasil.Generate (gen, DocSpec(DocSpec), DocType(Jupyter), docChoices)

import Drasil.Projectile.Lesson.Body (nb, printSetting)

main :: IO()
main = do
  setLocaleEncoding utf8
  gen (DocSpec (docChoices Jupyter []) "Projectile Lesson") nb printSetting
