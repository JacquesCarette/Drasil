module SysInfo.Drasil (
  -- * System Information
    SystemInformation(..)
  -- * Lenses
  , HasSystemInformation(..)
  -- * Reference Database
  , Purpose, Background, Scope, Motivation
  , citeDB
  -- ** Utility Helper Functions
  -- GetChunk
  , ccss, ccss', vars
) where

import SysInfo.Drasil.GetChunk
import SysInfo.Drasil.SystemInformation
