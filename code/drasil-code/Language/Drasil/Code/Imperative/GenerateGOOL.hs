module Language.Drasil.Code.Imperative.GenerateGOOL (ClassType(..),
  genModuleWithImports, genModule, genDoxConfig, primaryClass, auxClass, fApp, 
  ctorCall, fAppInOut
) where

import Language.Drasil
import Language.Drasil.Code.Imperative.DrasilState (DrasilState(..))
import Language.Drasil.Code.Imperative.GOOL.ClassInterface (AuxiliarySym(..))
import Language.Drasil.CodeSpec (CodeSpec(..), Comments(..))
import Language.Drasil.Mod (Name)
  
import GOOL.Drasil (Label, SFile, VSType, SVariable, SValue, MSStatement, 
  SMethod, CSStateVar, SClass, NamedArgs, ProgramSym, FileSym(..), 
  ValueExpression(..), FuncAppStatement(..), ClassSym(..), ModuleSym(..), 
  GOOLState)

import qualified Data.Map as Map (lookup)
import Data.Maybe (catMaybes)
import Control.Monad.Reader (Reader, ask, withReader)

genModuleWithImports :: (ProgramSym r) => Name -> String -> [String] -> 
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
                  (date g)
              | otherwise                                       = id
  return $ commMod $ fileDoc $ buildModule n is (catMaybes ms) (catMaybes cs)

genModule :: (ProgramSym r) => Name -> String -> 
  [Reader DrasilState (Maybe (SMethod r))] -> 
  [Reader DrasilState (Maybe (SClass r))] -> Reader DrasilState (SFile r)
genModule n desc = genModuleWithImports n desc []

genDoxConfig :: (AuxiliarySym r) => String -> GOOLState ->
  Reader DrasilState [r (Auxiliary r)]
genDoxConfig n s = do
  g <- ask
  let cms = commented g
      v = doxOutput g
  return [doxConfig n s v | not (null cms)]

data ClassType = Primary | Auxiliary

mkClass :: (ProgramSym r) => ClassType -> String -> Label -> Maybe Label -> 
  [CSStateVar r] -> Reader DrasilState [SMethod r] ->
  Reader DrasilState (SClass r)
mkClass s desc n i vs mths = do
  g <- ask
  ms <- mths
  let getFunc Primary = getFunc' i
      getFunc Auxiliary = extraClass n Nothing
      getFunc' Nothing = buildClass n Nothing
      getFunc' (Just intfc) = implementingClass n [intfc]
      f = getFunc s
  return $ if CommentClass `elem` commented g 
    then docClass desc (f vs ms) 
    else f vs ms

primaryClass :: (ProgramSym r) => String -> Label -> Maybe Label -> 
  [CSStateVar r] -> Reader DrasilState [SMethod r] -> 
  Reader DrasilState (SClass r)
primaryClass = mkClass Primary

auxClass :: (ProgramSym r) => String -> Label -> Maybe Label -> 
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
fApp :: (ProgramSym r) => String -> String -> VSType r -> [SValue r] -> 
  NamedArgs r -> Reader DrasilState (SValue r)
fApp m s t vl ns = do
  g <- ask
  let cm = currentModule g
  return $ if m /= cm then extFuncAppMixedArgs m s t vl ns else if Map.lookup s 
    (eMap g) == Just cm then funcAppMixedArgs s t vl ns else 
    selfFuncAppMixedArgs s t vl ns

-- Logic similar to fApp above, but self case not required here 
-- (because constructor will never be private)
ctorCall :: (ProgramSym r) => String -> VSType r -> [SValue r] -> NamedArgs r 
  -> Reader DrasilState (SValue r)
ctorCall m t vl ns = do
  g <- ask
  let cm = currentModule g
  return $ if m /= cm then extNewObjMixedArgs m t vl ns else 
    newObjMixedArgs t vl ns

-- Logic similar to fApp above
fAppInOut :: (ProgramSym r) => String -> String -> [SValue r] -> 
  [SVariable r] -> [SVariable r] -> Reader DrasilState (MSStatement r)
fAppInOut m n ins outs both = do
  g <- ask
  let cm = currentModule g
  return $ if m /= cm then extInOutCall m n ins outs both else if Map.lookup n
    (eMap g) == Just cm then inOutCall n ins outs both else 
    selfInOutCall n ins outs both
