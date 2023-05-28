-- | Re-export functions common to all SRS-related examples.
-- This aims to reduce clutter at the top of every @Body.hs@ file in the Drasil examples.
-- If an import is changed here, it will be changed for every example.
module Drasil.SRSDocument (
  -- * Chunk database types and functions
  -- | Imported from "Database.Drasil"
  Block(Parallel), ChunkDB, ReferenceDB, SystemInformation(..),
  cdb, rdb,
  -- * Printing Information needed to generate all documents
  -- | Imported from "Language.Drasil.Printers"
  PrintingInformation(..), defaultConfiguration, piSys,
  -- * Document section types needed for a SRS
  -- | Imported from "Drasil.DocDecl"
  SRSDecl, DocSection(..), ReqrmntSec(..), ReqsSub(..),
  PDSub(..), ProblemDescription(..), SSDSec(..), SSDSub(..), SCSSub(..),
  SolChSpec(..),
  -- ** Document subsection types needed for a SRS
  -- | Imported from "Drasil.DocumentLanguage.Core"
  AppndxSec(..), AuxConstntSec(..),
  DerivationDisplay(..), Emphasis(..), OffShelfSolnsSec(..), GSDSec(..),
  GSDSub(..), IntroSec(..), IntroSub(..), LFunc(..),
  Literature(..), RefSec(..), RefTab(..), StkhldrSec(..),
  StkhldrSub(..), TConvention(..), TraceabilitySec(..),
  TSIntro(..), TUIntro(..),
  -- ** Document subsection helper types
  -- | Imported from "Drasil.DocumentLanguage.Definitions"
  Field(..), Fields, InclUnits(IncludeUnits), Verbosity(..),
  -- * SRS Document creator functions
  -- | Imported from "Drasil.DocumentLanguage"
  mkDoc, fillcdbSRS,
  -- ** Helper functions to make an SRS Document
  -- | Imports from various sections of @drasil-docLang@
  intro,            -- Drasil.Sections.ReferenceMaterial
  traceMatStandard, -- Drasil.Sections.TraceabilityMandGs
  tsymb, tsymb'',   -- Drasil.Sections.TableOfSymbols
  purpDoc           -- Drasil.Sections.Introduction
  ) where

import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration, piSys)
import Database.Drasil
import SysInfo.Drasil
import Drasil.DocLang (
  -- Drasil.DocumentLanguage.Core
  AppndxSec(..), AuxConstntSec(..),
  DerivationDisplay(..), Emphasis(..), OffShelfSolnsSec(..), GSDSec(..),
  GSDSub(..), IntroSec(..), IntroSub(..), LFunc(..),
  Literature(..), RefSec(..), RefTab(..), StkhldrSec(..),
  StkhldrSub(..), TConvention(..), TraceabilitySec(..),
  TSIntro(..), TUIntro(..),
  -- Drasil.DocDecl
  SRSDecl, DocSection(..), ReqrmntSec(..), ReqsSub(..),
  PDSub(..), ProblemDescription(..), SSDSec(..), SSDSub(..), SCSSub(..),
  SolChSpec(..),
  -- Drasil.Sections.ReferenceMaterial
  intro,
  -- DocumentLanguage
  mkDoc, fillcdbSRS,
  -- Sections.TraceabilityMandGs
  traceMatStandard,
  -- Sections.TableOfSymbols
  tsymb, tsymb'',
  -- Sections.Introduction
  purpDoc,
  -- DocumentLanguage.Definitions
  Field(..), Fields, InclUnits(IncludeUnits), Verbosity(..))
  