-- | GOOL OOTest module. Various tests of general object functionality.
module GOOL.OOTest (ooTest) where

import Drasil.GOOL (OOProg, GSProgram, SMethod, ProgramSym(..), FileSym (..),
  ModuleSym (..), MethodSym (..), BodySym (..), IOStatement (..), Literal (..))
import Drasil.Metadata (watermark)
import Drasil.GProc (BlockSym(..))

ooTest :: (OOProg r) => GSProgram r
ooTest = prog "OOTest" "Tests various aspects of general object functionality"
  [docMod "Tests various aspects of general object functionality" watermark
  ["Brandon Bosman"] "Apr. 24, 2026" (fileDoc (buildModule "OOTest" [] [ooTestMain] []))]

ooTestMain :: (OOProg r) => SMethod r
ooTestMain = mainFunction (body [block [printLn $ litString "Hello World"]])
