-- Re-exports drasil-file-handling to avoid dependency issues
-- in the drasil-file-handling tests.
module Drasil.TestingKit.FileHandling
  ( module Drasil.FileHandling
  )
where

import Drasil.FileHandling
