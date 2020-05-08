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

type ODEGenInfo = (Maybe FilePath, [(Name, ExtLibState)])

chooseODELib :: Lang -> [ODELibPckg] -> [ODEInfo] -> ODEGenInfo
chooseODELib _ _ [] = (Nothing, [])
chooseODELib l olps odes = chooseODELib' olps
  where chooseODELib' [] = error $ "None of the chosen ODE libraries are " ++ 
          "compatible with " ++ show l
        chooseODELib' (o:os) = if l `elem` compatibleLangs o 
          then (libPath o, map (\ode -> (codeName $ odeDef ode, 
            genExternalLibraryCall (libSpec o) $ libCall o ode)) odes) 
          else chooseODELib' os