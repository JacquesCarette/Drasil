module Drasil.Projectile.Lesson.Main (main) where

import GHC.IO.Encoding

import Drasil.Generator (genDoc, DocSpec(DocSpec), DocType(Lesson), docChoices)

import Drasil.Projectile.Lesson.Body (nb, printSetting)

main :: IO()
main = do
  setLocaleEncoding utf8
  genDoc (DocSpec (docChoices Lesson []) "Projectile_Lesson") nb printSetting
