-- | Defines a type for representing ODE-solving external libraries.
module Language.Drasil.Data.ODELibPckg (
  ODELibPckg(..), mkODELib, mkODELibNoPath
) where

import Language.Drasil.Code.ExternalLibrary (ExternalLibrary)
import Language.Drasil.Code.ExternalLibraryCall (ExternalLibraryCall)
import Language.Drasil.Code.Lang (Lang)
import Language.Drasil.Data.ODEInfo (ODEInfo)
import Language.Drasil.Mod (Name, Version)

-- | Holds an ODE library package.
data ODELibPckg = ODELib {
  -- | Library name.
  libName :: Name,
  -- | Version.
  libVers :: Version,
  -- | Library specifications.
  libSpec :: ExternalLibrary,
  -- | Library call.
  libCall :: ODEInfo -> ExternalLibraryCall,
  -- | Library path.
  libPath :: Maybe FilePath,
  -- It has been said that language information should be in language-related
  -- file, but this needs more thought. The language would need to declare
  -- which libraries it is compatible with, but how could it refer to a
  -- library? Give libraries UID?
  -- Also, this case seems different because ExternalLibraries are data. In
  -- future, if a user defines a new external library, we wouldn't expect them
  -- to update an internal Drasil file to add their library to a language's
  -- compatible libraries. So maybe declaring compatible languages at library
  -- definition time really is the right way to do this.
  -- | Compatible OO languages.
  compatibleLangs :: [Lang]
}

-- | Makes an 'ODELibPckg' with the given name, 'ExternalLibrary' specification,
-- 'ExternalLibraryCall' specification parameterized by an 'ODEInfo', local file
-- path to the library, and list of compatible languages.
mkODELib :: Name -> Version -> ExternalLibrary -> (ODEInfo ->
  ExternalLibraryCall) -> FilePath -> [Lang] -> ODELibPckg
mkODELib n v e c f = ODELib n v e c (Just f)

-- | Makes an 'ODELibPckg' with the given name, 'ExternalLibrary' specification,
-- 'ExternalLibraryCall' specification parameterized by an 'ODEInfo', and list of
-- compatible languages.
mkODELibNoPath :: Name -> Version -> ExternalLibrary -> (ODEInfo ->
  ExternalLibraryCall) -> [Lang] -> ODELibPckg
mkODELibNoPath n v e c = ODELib n v e c Nothing
