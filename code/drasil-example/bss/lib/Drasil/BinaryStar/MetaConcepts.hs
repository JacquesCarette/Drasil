module Drasil.BinaryStar.MetaConcepts (progName) where

import Drasil.Database (mkUid)
import Language.Drasil

import Data.Drasil.Domains (physics)

progName :: CI
progName = commonIdeaWithDict (mkUid "bss")
  (pn "Binary Star System Simulator") "BSS" [physics]
