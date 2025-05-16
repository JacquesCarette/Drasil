module SysInfo.Drasil (
  -- * System Information
    System(..)
  -- * Lenses
  , HasSystem(..)
  -- * Reference Database
  , Purpose, Background, Scope, Motivation
  , citeDB
  -- ** Utility Helper Functions
  -- GetChunk
  , ccss, ccss', vars
) where

import SysInfo.Drasil.GetChunk
import SysInfo.Drasil.SystemInformation
