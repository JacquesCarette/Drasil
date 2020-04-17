module Language.Drasil.Code.Imperative.GenerateGOOL (ClassType(..),
  genModuleWithImports, genModule, genDoxConfig, primaryClass, auxClass, fApp, 
  ctorCall, fAppInOut, mkParam
) where

import Language.Drasil
import Language.Drasil.Code.Imperative.DrasilState (DrasilState(..))
import Language.Drasil.Code.Imperative.GOOL.ClassInterface (AuxiliarySym(..))
import Language.Drasil.CodeSpec (CodeSpec(..), CodeSystInfo(..), Comments(..))
import Language.Drasil.Mod (Name, Description, Import)
  
import GOOL.Drasil (SFile, VSType, SVariable, SValue, MSStatement, 
  MSParameter, SMethod, CSStateVar, SClass, NamedArgs, OOProg, FileSym(..), 
  TypeElim(..), VariableElim(..), ValueExpression(..), FuncAppStatement(..), 
  ParameterSym(..), ClassSym(..), ModuleSym(..), CodeType(..), GOOLState, 
  lensMStoVS)

import Control.Lens.Zoom (zoom)
import qualified Data.Map as Map (lookup)
import Data.Maybe (catMaybes)
import Control.Monad.Reader (Reader, ask, withReader)

genModuleWithImports :: (OOProg r) => Name -> Description -> [Import] -> 
  [Reader DrasilState (Maybe (SMethod r))] -> 
  [Reader DrasilState (Maybe (SClass r))] -> Reader DrasilState (SFile r)
genModuleWithImports n desc is maybeMs maybeCs = do
  g <- ask
  let updateState = withReader (\s -> s { currentModule = n })
      -- Below line of code cannot be simplified because authors has a generic type
      as = case csi (codeSpec g) of CSI {authors = a} -> map name a
  cs <- mapM updateState maybeCs
  ms <- mapM updateState maybeMs
  let commMod | CommentMod `elem` commented g                   = docMod desc 
                  as (date g)
              | CommentFunc `elem` commented g && not (null ms) = docMod "" []  
                  (date g)
              | otherwise                                       = id
  return $ commMod $ fileDoc $ buildModule n is (catMaybes ms) (catMaybes cs)

genModule :: (OOProg r) => Name -> Description -> 
  [Reader DrasilState (Maybe (SMethod r))] -> 
  [Reader DrasilState (Maybe (SClass r))] -> Reader DrasilState (SFile r)
genModule n desc = genModuleWithImports n desc []

genDoxConfig :: (AuxiliarySym r) => Name -> GOOLState ->
  Reader DrasilState [r (Auxiliary r)]
genDoxConfig n s = do
  g <- ask
  let cms = commented g
      v = doxOutput g
  return [doxConfig n s v | not (null cms)]

data ClassType = Primary | Auxiliary

mkClass :: (OOProg r) => ClassType -> Name -> Maybe Name -> Description -> 
  [CSStateVar r] -> Reader DrasilState [SMethod r] ->
  Reader DrasilState (SClass r)
mkClass s n l desc vs mths = do
  g <- ask
  ms <- withReader (\ds -> ds {currentClass = n}) mths
  let getFunc Primary = buildClass
      getFunc Auxiliary = extraClass
      f = getFunc s
  return $ if CommentClass `elem` commented g 
    then docClass desc (f n l vs ms) 
    else f n l vs ms

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
    (eMap $ codeSpec g) == Just cm then funcAppMixedArgs s t vl ns else 
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
    (eMap $ codeSpec g) == Just cm then inOutCall n ins outs both else 
    selfInOutCall n ins outs both

mkParam :: (OOProg r) => SVariable r -> MSParameter r
mkParam v = zoom lensMStoVS v >>= (\v' -> paramFunc (getType $ variableType v') 
  v)
  where paramFunc (List _) = pointerParam
        paramFunc (Object _) = pointerParam
        paramFunc _ = param