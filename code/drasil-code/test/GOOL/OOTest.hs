-- | GOOL OOTest module. Various tests of general object functionality.
module GOOL.OOTest (ooTest) where

import Drasil.GOOL (OOProg, GSProgram, ProgramSym(..))

ooTest :: (OOProg r) => GSProgram r
ooTest = prog "OOTest" "Tests various aspects of general object functionality"
    []