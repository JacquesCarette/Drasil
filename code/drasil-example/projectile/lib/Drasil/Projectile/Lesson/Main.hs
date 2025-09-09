module Drasil.Projectile.Lesson.Main (main) where

import GHC.IO.Encoding

import Drasil.Generator (gen, DocSpec(DocSpec), DocType(Lesson), docChoices)

import Drasil.Projectile.Lesson.Body (nb, printSetting)

main :: IO()
main = do
  setLocaleEncoding utf8
  gen (DocSpec (docChoices Lesson []) "Projectile_Lesson") nb printSetting
