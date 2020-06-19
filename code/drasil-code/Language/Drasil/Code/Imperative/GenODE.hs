module Language.Drasil.Code.Imperative.GenODE (
  chooseODELib
) where

import Language.Drasil.Code.ExtLibImport (ExtLibState(..), 
  genExternalLibraryCall)
import Language.Drasil.Code.Lang (Lang(..))
import Language.Drasil.Chunk.Code (codeName)
import Language.Drasil.Chunk.CodeDefinition (odeDef)
import Language.Drasil.Mod (Name)
import Language.Drasil.Data.ODEInfo (ODEInfo)
import Language.Drasil.Data.ODELibPckg (ODELibPckg(..))

import Control.Monad.State (State, modify)
import Text.PrettyPrint.HughesPJ (Doc, ($$), text)

type ODEGenInfo = (Maybe FilePath, [(Name, ExtLibState)], Name)

-- Chooses the first ODELibPckg from the list specified by the user that is 
-- compatible with the current target Lang. 
-- Interprets the ExternalLibrary and ExternalLibraryCall for the selected 
-- ODELibPckg by concretizing the ExternalLibraryCall with each of the ODEInfos
chooseODELib :: Lang -> [ODELibPckg] -> [ODEInfo] -> State Doc ODEGenInfo
chooseODELib _ _ [] = return (Nothing, [], "")
chooseODELib l olps odes = chooseODELib' olps
  where chooseODELib' :: [ODELibPckg] -> State Doc ODEGenInfo
        chooseODELib' [] = error $ "None of the chosen ODE libraries are " ++ 
          "compatible with " ++ show l
        chooseODELib' (o:os) = if l `elem` compatibleLangs o 
          then return (libPath o, map (\ode -> (codeName $ odeDef ode, 
            genExternalLibraryCall (libSpec o) $ libCall o ode)) odes, libName o) 
          else modify ($$ incompatibleLib l o) >> chooseODELib' os

-- Defines a design log message based on an incompatibility between the given 
-- Lang and chosen ODELibPckg.
incompatibleLib :: Lang -> ODELibPckg -> Doc
incompatibleLib lng lib = text $ "Language " ++ show lng ++ " is not " ++ 
  "compatible with chosen library " ++ libName lib ++ ", trying next choice." 