module Main (main) where

import Language.Drasil.Code (Choices(..), CodeSpec, Comments(..), 
  ConstraintBehaviour(..), ImplementationType(..), Lang(..), Logging(..), 
  Structure(..), codeSpec)
import Language.Drasil.Generate (gen, genCode)
import Language.Drasil.Printers (DocSpec(DocSpec), DocType(SRS, Website))

import Drasil.Projectile.Body (printSetting, srsDoc, systInfo)

code :: CodeSpec
code = codeSpec systInfo choices []

choices :: Choices
choices = Choices {
  lang = [Python, Cpp, CSharp, Java],
  impType = Program,
  logFile = "log.txt",
  logging = LogNone,
  comments = CommentNone,
  onSfwrConstraint = Warning,
  onPhysConstraint = Warning,
  inputStructure = Bundled
}

main :: IO()
main = do
  gen (DocSpec SRS     "Projectile_SRS") srsDoc printSetting
  gen (DocSpec Website "Projectile_SRS") srsDoc printSetting
  genCode choices code
