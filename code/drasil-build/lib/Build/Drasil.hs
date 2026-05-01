-- | Re-export all contents in drasil-build for external use.
module Build.Drasil (
  -- * Makefiles
  -- ** Types
    Annotation, Dependencies, Makefile, Command, Rule
  -- ** Constructors
  , mkCheckedCommand, mkCommand, mkFile, mkMakefile, mkRule
    -- MakeString
  , MakeString, (+:+), makeS, mkFreeVar, mkImplicitVar, mkWindowsVar, mkOSVar
    -- ** Printers
  , printMakefile
) where

import Build.Drasil.Make.AST (Annotation, Command, Dependencies, mkCheckedCommand,
  mkCommand, mkFile, mkMakefile, mkRule, Makefile, Rule)
import Build.Drasil.Make.MakeString ((+:+), makeS, MakeString, mkFreeVar,
  mkImplicitVar, mkWindowsVar, mkOSVar)
import Build.Drasil.Make.Print (printMakefile)
