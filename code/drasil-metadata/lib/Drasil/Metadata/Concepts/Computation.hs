-- | Defines concepts used in computing, to be used internally.
module Drasil.Metadata.Concepts.Computation (algorithm, defaultOSs, linux,
  macOS, windows) where

import Language.Drasil (cn, cn', ConceptChunk, cncpt''', IdeaDict, idea',
  Sentence(..))
import Drasil.Database (mkUid)

algorithm :: ConceptChunk
algorithm = cncpt''' (mkUid "algorithm") (cn' "algorithm")
  (S "a series of steps to be followed in calculations and problem-solving operations")

-- * Operating Systems

linux, macOS, windows :: IdeaDict
linux   = idea' (mkUid "linux")   (cn "Linux")
macOS   = idea' (mkUid "macOS")   (cn "macOS")
windows = idea' (mkUid "windows") (cn "Windows")

-- | The operating systems that Drasil's generated software targets by default.
defaultOSs :: [IdeaDict]
defaultOSs = [windows, macOS, linux]
