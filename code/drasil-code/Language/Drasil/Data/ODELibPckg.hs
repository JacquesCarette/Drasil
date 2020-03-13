module Language.Drasil.Data.ODELibPckg (
  ODELibPckg(..), mkODELib
) where

import Language.Drasil

import Language.Drasil.Code.ExternalLibrary (ExternalLibrary)
import Language.Drasil.Code.ExternalLibraryCall (ExternalLibraryCall)
import Language.Drasil.Code.Lang (Lang)
import Language.Drasil.Data.ODEInfo (ODEInfo)

data ODELibPckg = ODELib {
  libSpec :: ExternalLibrary,
  libCall :: ODEInfo -> ExternalLibraryCall,
  -- It has been said that language information should be in language-related 
  -- file, but this needs more thought. The language would need to declare 
  -- which libraries it is compatible with, but how could it refer to a 
  -- library? Give libraries UID?
  -- Also, this case seems different because ExternalLibraries are data. In 
  -- future, if a user defines a new external library, we wouldn't expect them 
  -- to update an internal Drasil file to add their library to a language's 
  -- compatible libraries. So maybe declaring compatible languages at library 
  -- definition time really is the right way to do this.
  compatibleLangs :: [Lang]
}

mkODELib :: ExternalLibrary -> (ODEInfo -> ExternalLibraryCall) -> [Lang] -> 
  ODELibPckg
mkODELib = ODELib