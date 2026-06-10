module Drasil.BinaryStar.MetaConcepts (progName) where

import Language.Drasil

import Data.Drasil.Domains (physics)

progName :: CI
progName = commonIdeaWithDict "bss"
  (pn "Binary Star System Simulator") "BSS" [physics]
