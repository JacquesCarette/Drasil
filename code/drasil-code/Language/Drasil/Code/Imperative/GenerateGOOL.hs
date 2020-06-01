module Language.Drasil.Code.Imperative.GenerateGOOL (ClassType(..),
  genModuleWithImports, genModule, genDoxConfig, primaryClass, auxClass, fApp, 
  ctorCall, fAppInOut
) where

import Language.Drasil
import Language.Drasil.Code.Imperative.DrasilState (DrasilState(..))
import Language.Drasil.Code.Imperative.GOOL.ClassInterface (AuxiliarySym(..))
import Language.Drasil.Choices (Comments(..))
import Language.Drasil.CodeSpec (CodeSpec(..))
import Language.Drasil.Mod (Name, Description, Import)
  
import GOOL.Drasil (SFile, VSType, SVariable, SValue, MSStatement, SMethod, 
  CSStateVar, SClass, NamedArgs, OOProg, FileSym(..), ValueExpression(..), 
  FuncAppStatement(..), ClassSym(..), ModuleSym(..), GOOLState)

import qualified Data.Map as Map (lookup)
import Data.Maybe (catMaybes)
import Control.Monad.Reader (Reader, ask, withReader)

-- | Defines a GOOL module. If the user chose CommentMod, the module will have
-- Doxygen comments. If the user did not choose CommentMod but did choose 
-- CommentFunc, a module-level Doxygen comment is still created, though it only 
-- documents the file name, because without this Doxygen will not find the 
-- function-level comments in the file.
genModuleWithImports :: (OOProg r) => Name -> Description -> [Import] -> 
  [Reader DrasilState (Maybe (SMethod r))] -> 
  [Reader DrasilState (Maybe (SClass r))] -> Reader DrasilState (SFile r)
genModuleWithImports n desc is maybeMs maybeCs = do
  g <- ask
  let updateState = withReader (\s -> s { currentModule = n })
      -- Below line of code cannot be simplified because authors has a generic type
      as = case codeSpec g of CodeSpec {authors = a} -> map name a
  cs <- mapM updateState maybeCs
  ms <- mapM updateState maybeMs
  let commMod | CommentMod `elem` commented g                   = docMod desc 
                  as (date g)
              | CommentFunc `elem` commented g && not (null ms) = docMod "" []  
                  ""
              | otherwise                                       = id
  return $ commMod $ fileDoc $ buildModule n is (catMaybes ms) (catMaybes cs)

-- | Generates a module for when imports do not need to be explicitly stated
genModule :: (OOProg r) => Name -> Description -> 
  [Reader DrasilState (Maybe (SMethod r))] -> 
  [Reader DrasilState (Maybe (SClass r))] -> Reader DrasilState (SFile r)
genModule n desc = genModuleWithImports n desc []

genDoxConfig :: (AuxiliarySym r) => GOOLState ->
  Reader DrasilState [r (Auxiliary r)]
genDoxConfig s = do
  g <- ask
  let n = pName $ codeSpec g
      cms = commented g
      v = doxOutput g
  return [doxConfig n s v | not (null cms)]

data ClassType = Primary | Auxiliary

mkClass :: (OOProg r) => ClassType -> Name -> Maybe Name -> Description -> 
  [CSStateVar r] -> Reader DrasilState [SMethod r] ->
  Reader DrasilState (SClass r)
mkClass s n l desc vs mths = do
  g <- ask
  ms <- withReader (\ds -> ds {currentClass = n}) mths
  let getFunc Primary = getFunc' l
      getFunc Auxiliary = extraClass n Nothing
      getFunc' Nothing = buildClass Nothing
      getFunc' (Just intfc) = implementingClass n [intfc]
      c = getFunc s vs ms
  return $ if CommentClass `elem` commented g 
    then docClass desc c
    else c

primaryClass :: (OOProg r) => Name -> Maybe Name -> Description -> 
  [CSStateVar r] -> Reader DrasilState [SMethod r] -> 
  Reader DrasilState (SClass r)
primaryClass = mkClass Primary

auxClass :: (OOProg r) => Name -> Maybe Name -> Description -> 
  [CSStateVar r] -> Reader DrasilState [SMethod r] -> 
  Reader DrasilState (SClass r)
auxClass = mkClass Auxiliary

-- m parameter is the module where the function is defined
-- if m is not current module, use GOOL's function for calling functions from 
--   external modules
-- if m is current module and the function is in export map, use GOOL's basic 
--   function for function applications
-- if m is current module and function is not exported, use GOOL's function for 
--   calling a method on self. This assumes all private methods are dynamic, 
--   which is true for this generator.
fApp :: (OOProg r) => Name -> Name -> VSType r -> [SValue r] -> 
  NamedArgs r -> Reader DrasilState (SValue r)
fApp m s t vl ns = do
  g <- ask
  let cm = currentModule g
  return $ if m /= cm then extFuncAppMixedArgs m s t vl ns else if Map.lookup s 
    (eMap g) == Just cm then funcAppMixedArgs s t vl ns else 
    selfFuncAppMixedArgs s t vl ns

-- Logic similar to fApp above, but self case not required here 
-- (because constructor will never be private)
ctorCall :: (OOProg r) => Name -> VSType r -> [SValue r] -> NamedArgs r 
  -> Reader DrasilState (SValue r)
ctorCall m t vl ns = do
  g <- ask
  let cm = currentModule g
  return $ if m /= cm then extNewObjMixedArgs m t vl ns else 
    newObjMixedArgs t vl ns

-- Logic similar to fApp above
fAppInOut :: (OOProg r) => Name -> Name -> [SValue r] -> 
  [SVariable r] -> [SVariable r] -> Reader DrasilState (MSStatement r)
fAppInOut m n ins outs both = do
  g <- ask
  let cm = currentModule g
  return $ if m /= cm then extInOutCall m n ins outs both else if Map.lookup n
    (eMap g) == Just cm then inOutCall n ins outs both else 
    selfInOutCall n ins outs both
