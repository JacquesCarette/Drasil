module Language.Drasil.Code.Imperative.GenODE (
  chooseODELib
) where

import Language.Drasil (Sentence(..), (+:+.))
import Language.Drasil.Code.ExtLibImport (ExtLibState(..), 
  genExternalLibraryCall)
import Language.Drasil.Code.Lang (Lang(..))
import Language.Drasil.Chunk.Code (codeName)
import Language.Drasil.Chunk.CodeDefinition (odeDef)
import Language.Drasil.Mod (Name, Version)
import Language.Drasil.Data.ODEInfo (ODEInfo)
import Language.Drasil.Data.ODELibPckg (ODELibPckg(..))

import Control.Monad.State (State, modify)

type ODEGenInfo = (Maybe FilePath, [(Name, ExtLibState)], (Name,Version))

-- Chooses the first ODELibPckg from the list specified by the user that is 
-- compatible with the current target Lang. 
-- Interprets the ExternalLibrary and ExternalLibraryCall for the selected 
-- ODELibPckg by concretizing the ExternalLibraryCall with each of the ODEInfos
-- chooseODELib' is keeps a read only preference list and a currently considered
-- preference list (which can change), this facilitates the firstChoiceODELib check
chooseODELib :: Lang -> [ODELibPckg] -> [ODEInfo] -> State [Sentence] ODEGenInfo
chooseODELib _ _ [] = return (Nothing, [], ("",""))
chooseODELib l olps odes = chooseODELib' olps olps
  where chooseODELib' :: [ODELibPckg] -> [ODELibPckg] -> State [Sentence] ODEGenInfo
        chooseODELib' _ [] = error $ "None of the chosen ODE libraries are " ++ 
          "compatible with " ++ show l
        chooseODELib' prefLibList (o:os) = if l `elem` compatibleLangs o 
          then do 
            modify (++ [firstChoiceODELib prefLibList o])
            return (libPath o, map (\ode -> (codeName $ odeDef ode, 
              genExternalLibraryCall (libSpec o) $ libCall o ode)) odes, 
                (libName o, libVers o)) 
          else modify (++ [incompatibleLib l o]) >> chooseODELib' prefLibList os

-- Defines a design log message based on an incompatibility between the given 
-- Lang and chosen ODELibPckg.
incompatibleLib :: Lang -> ODELibPckg -> Sentence
incompatibleLib lng lib = S $ "Language " ++ show lng ++ " is not " ++ 
  "compatible with chosen library " ++ libName lib ++ ", trying next choice." 

-- Defines a design log message if the first choice ODE Library, which is the head of
-- the preference list that the user selected, is compatible with the given Lang.
firstChoiceODELib :: [ODELibPckg] -> ODELibPckg -> Sentence
firstChoiceODELib prefer olp =  if libName (head prefer) == libName olp  then 
  S "Successfully selected first choice ODE Library package" +:+. S (libName olp)  
  else S "ODE Library package selected as" +:+. S (libName olp) 