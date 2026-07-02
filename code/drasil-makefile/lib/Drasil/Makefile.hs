-- | Re-export all contents in drasil-makefile for external use.
module Drasil.Makefile (
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

import Drasil.Makefile.Make.AST (Annotation, Command, Dependencies, mkCheckedCommand,
  mkCommand, mkFile, mkMakefile, mkRule, Makefile, Rule)
import Drasil.Makefile.Make.MakeString ((+:+), makeS, MakeString, mkFreeVar,
  mkImplicitVar, mkWindowsVar, mkOSVar)
import Drasil.Makefile.Make.Print (printMakefile)
